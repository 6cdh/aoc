struct Stone
    h::Int
    w::Int
    pos::Vector{Vector{Int}}
end

function is_overlap(pos, stone, room)
    any(stone.pos) do p
        room_pos = pos + p
        room_pos[2] <= 0 || room_pos[2] >= 8 ||
            room_pos[1] == 0 ||
            (room_pos[1] <= length(room) &&
             room[room_pos[1]][room_pos[2]] == '█')
    end
end

function move(pos, dir, stone, room)
    new_pos = pos + dir
    if is_overlap(new_pos, stone, room)
        pos, false
    else
        new_pos, true
    end
end

function draw_stone(j, jets, stone, room)
    pos = [length(room) + 3 + stone.h, 3]
    while true
        jet = jets[j]
        j = j % length(jets) + 1
        if jet == '>'
            pos, _ = move(pos, [0, 1], stone, room)
        else
            pos, _ = move(pos, [0, -1], stone, room)
        end
        pos, succ = move(pos, [-1, 0], stone, room)

        if !succ
            while length(room) < pos[1]
                push!(room, fill('.', 7))
            end
            for p in stone.pos
                room_pos = pos + p
                room[room_pos[1]][room_pos[2]] = '█'
            end
            return j
        end
    end
end

function faster_simulate(i, j, s, n, jets, stones, room, lens, mem, matched, queries)
    if i <= n
        new_j = draw_stone(j, jets, stones[s], room)

        mem_key = (j, s, last(room))
        if haskey(mem, mem_key) && matched
            # cycle interval [from, to]
            cycle_from = mem[mem_key]
            cycle_to = i - 1
            cycle_len = i - cycle_from

            map(queries) do q
                repeats, rem = divrem(q - (cycle_from - 1), cycle_len)

                lens[cycle_from-1] +
                repeats * (lens[cycle_to] - lens[cycle_from-1]) +
                lens[cycle_from+rem-1] - lens[cycle_from-1]
            end
        else
            matched = haskey(mem, mem_key)
            mem[mem_key] = i
            push!(lens, length(room))
            faster_simulate(
                i + 1, new_j, s % length(stones) + 1,
                n, jets, stones, room, lens, mem, matched, queries)
        end
    else
        map(q -> lens[q], queries)
    end
end

function simulate(jets, stones, queries)
    room::Vector{Vector{Char}} = []
    lens::Vector{Int} = []
    mem::Dict{Tuple{Int,Int,Vector{Char}},Int} = Dict()
    faster_simulate(1, 1, 1, maximum(queries), jets, stones, room, lens, mem, false, queries)
end

function day17()
    jets = readline()
    stones = [
        Stone(1, 4, [[0, 0], [0, 1], [0, 2], [0, 3]]),
        Stone(3, 3, [[0, 1], [-1, 0], [-1, 1], [-1, 2], [-2, 1]]),
        Stone(3, 3, [[0, 2], [-1, 2], [-2, 0], [-2, 1], [-2, 2]]),
        Stone(4, 1, [[0, 0], [-1, 0], [-2, 0], [-3, 0]]),
        Stone(2, 2, [[0, 0], [0, 1], [-1, 0], [-1, 1]]),
    ]

    res1, res2 = simulate(jets, stones, [2022, 1000_000_000_000])
    println(res1)
    println(res2)
end

@time day17()
