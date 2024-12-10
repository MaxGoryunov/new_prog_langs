use std::thread;

fn replace_if_divisible<T, F>(vec: &mut Vec<T>, condition: F, new_value: T)
where
    F: Fn(&T) -> bool + Send + Sync + Clone + 'static,
    T: Copy + Send + 'static,
{
    let mut handles = vec![];
    let len = vec.len();

    for i in 0..len {
        let condition_clone = condition.clone(); 
        let new_value_clone = new_value;
        let vec_clone = vec.clone();

        let handle = thread::spawn(move || {
            let mut vec_clone = vec_clone;
            if condition_clone(&vec_clone[i]) {
                vec_clone[i] = new_value_clone;
            }
            vec_clone
        });

        handles.push(handle);
    }

    for handle in handles {
        let result = handle.join().unwrap();
        for i in 0..len {
            if condition(&result[i]) {
                vec[i] = new_value;
            }
        }
    }
}

fn main() {
    let mut numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    replace_if_divisible(&mut numbers, |&x| x % 3 == 0, 30);

    println!("{:?}", numbers);
}
