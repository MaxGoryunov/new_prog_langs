function tower_of_hanoi(n, source, target, auxiliary)
    if n == 1 then
        print("Move disk 1 from " .. source .. " to " .. target)
        return
    end

    tower_of_hanoi(n - 1, source, auxiliary, target)
    print("Move disk " .. n .. " from " .. source .. " to " .. target)
    tower_of_hanoi(n - 1, auxiliary, target, source)
end


function main()
    local num_disks = 4
    tower_of_hanoi(num_disks, "A", "C", "B")
end


main()