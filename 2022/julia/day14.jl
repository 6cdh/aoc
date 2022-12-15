function add_line!(from, to, barriers)
    for xi in min(from[1], to[1]):max(from[1], to[1])
        for yi in min(from[2], to[2]):max(from[2], to[2])
            push!(barriers, (xi, yi))
        end
    end
end

function read_barriers()
    lines = readlines()

    floor = 0
    barriers::Set{Tuple{Int,Int}} = Set()
    for line in lines
        ints = parse.(Int, split(line, x -> x in " ->,"; keepempty=false))
        for i in 3:2:length(ints)
            from = (ints[i-2], ints[i-1])
            to = (ints[i], ints[i+1])
            floor = max(floor, from[2], to[2])
            add_line!(from, to, barriers)
        end
    end
    (barriers, floor)
end

function trace_sand(poses, floor, barriers, isinf)
    pos = last(poses)
    if pos[2] == floor # with abyss
        poses
    elseif isinf && pos[2] == floor - 1 # with infinity floor
        poses
    else
        for dx in (0, -1, 1)
            next = (pos[1] + dx, pos[2] + 1)
            if next ∉ barriers
                push!(poses, next)
                return trace_sand(poses, floor, barriers, isinf)
            end
        end
        poses
    end
end

function emulate(barriers, floor, isinf)
    source = (500, 0)
    cnt = 0
    poses = [source]
    while true
        poses = trace_sand(poses, floor, barriers, isinf)

        pos = last(poses)
        if pos[2] == floor # with abyss
            return cnt
        elseif pos == source # with infinity floor
            return cnt + 1
        end
        push!(barriers, pos)

        pop!(poses)
        cnt += 1
    end
end

function day14()
    barriers, floor = read_barriers()

    res1 = emulate(barriers, floor, false)
    println(res1)
    println(emulate(barriers, floor + 2, true) + res1)
end

@time day14()
