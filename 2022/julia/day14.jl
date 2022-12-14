function read_barriers()
    lines = readlines()

    floor = 0
    barriers::Set{Tuple{Int,Int}} = Set()
    for line in lines
        ints = parse.(Int, split(line, x -> x in " ->,"; keepempty=false))
        for i in 3:2:length(ints)
            fromx, fromy = ints[i-2], ints[i-1]
            tox, toy = ints[i], ints[i+1]
            floor = max(floor, fromy, toy)
            for xi in min(fromx, tox):max(fromx, tox)
                for yi in min(fromy, toy):max(fromy, toy)
                    push!(barriers, (xi, yi))
                end
            end
        end
    end
    (barriers, floor)
end

function emulate(barriers, floor, isinf)
    source = (500, 0)
    cnt = 0

    function fall_sand(pos)
        if pos[2] == floor
            pos
        elseif isinf && pos[2] == floor - 1
            pos
        else
            for dx in (0, -1, 1)
                next = (pos[1] + dx, pos[2] + 1)
                if next ∉ barriers
                    return fall_sand(next)
                end
            end
            pos
        end
    end

    while true
        pos = fall_sand(source)
        if pos[2] == floor
            return cnt
        elseif pos == source
            return cnt + 1
        end
        push!(barriers, pos)
        cnt += 1
    end
end

function day14()
    barriers, floor = read_barriers()

    println(emulate(copy(barriers), floor, false))
    println(emulate(copy(barriers), floor + 2, true))
end

@time day14()
