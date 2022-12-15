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

function emulate(node, barriers, floor, recode)
    if node ∉ barriers && node[2] < floor
        recode(node, barriers)
        for dx in (0, -1, 1)
            emulate((node[1] + dx, node[2] + 1), barriers, floor, recode)
        end
        push!(barriers, node)
    end
end

function day14()
    barriers, floor = read_barriers()

    rocks = length(barriers)
    first_abyss = -1

    emulate((500, 0), barriers, floor + 2,
        (node, bs) -> begin
            if node[2] == floor && first_abyss == -1
                first_abyss = length(bs) - rocks
            end
        end)

    println(first_abyss)
    println(length(barriers) - rocks)
end

@time day14()
