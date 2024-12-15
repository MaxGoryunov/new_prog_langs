use std::thread;
use std::time::Instant;

fn replace_if_divisible<T, F>(vec: &mut Vec<T>, condition: F, new_value: T)
where
    F: Fn(&T) -> bool + Send + Clone + 'static,
    T: Copy + Send + 'static,
{
    let mut handles = vec![];
    let len = vec.len();

    let num_threads = 8;
    let chunk_size = (len + num_threads - 1) / num_threads; 
    
    for chunk in 0..num_threads {
        let start = chunk * chunk_size;
        let end = ((chunk + 1) * chunk_size).min(len);
        
        if start >= len {
            break;
        }

        let condition_clone = condition.clone();
        let new_value_clone = new_value;

        let chunk_vec = vec[start..end].to_vec();

        let handle = thread::spawn(move || {
            let mut local_chunk = chunk_vec;
            for i in 0..local_chunk.len() {
                if condition_clone(&local_chunk[i]) {
                    local_chunk[i] = new_value_clone;
                }
            }
            local_chunk
        });

        handles.push(handle);
    }

    for (chunk, handle) in handles.into_iter().enumerate() {
        let result = handle.join().unwrap();
        let start = chunk * chunk_size;
        let _end = ((chunk + 1) * chunk_size).min(len);
        for i in 0..result.len() {
            vec[start + i] = result[i];
        }
    }
}

fn replace_if_divisible_non_parallel<T, F>(vec: &mut Vec<T>, condition: F, new_value: T)
where
    F: Fn(&T) -> bool,
    T: Copy,
{
    let len = vec.len();

    for i in 0..len {
        if condition(&vec[i]) {
            vec[i] = new_value;
        }
    }
}

fn main() {
    let boundary = 1000 * 500; 
    let mut numbers_parallel: Vec<i32> = (1..=boundary).collect();
    let mut numbers_non_parallel = numbers_parallel.clone();

    let start = Instant::now();
    replace_if_divisible(&mut numbers_parallel, |&x| x % 3 == 0, 30);
    let duration_parallel = start.elapsed();

    let start = Instant::now();
    replace_if_divisible_non_parallel(&mut numbers_non_parallel, |&x| x % 3 == 0, 30);
    let duration_non_parallel = start.elapsed();

    println!("Parallel execution time: {:?}", duration_parallel);
    println!("Non-parallel execution time: {:?}", duration_non_parallel);
    //println!("Parallel result: {:?}", numbers_parallel);
    //println!("Non-parallel result: {:?}", numbers_non_parallel);
}
