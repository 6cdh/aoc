function move_pos(t1::Tuple{Int,Int}, t2::Tuple{Int,Int})
    (t1[1] + t2[1], t1[2] + t2[2])
end

function simulate1(elves, dirs, surrounding)
    new_elves::Vector{NTuple{2,Int}} = copy(elves)
    elf_set::Set{NTuple{2,Int}} = Set(elves)
    n = length(elves)

    for e in 1:n
        elf = elves[e]
        has_elf = any(surrounding) do Δ
            pos = move_pos(elf, Δ)
            pos ∈ elf_set
        end

        if has_elf
            for dir3 in dirs
                no_elf = all(dir3) do d
                    pos = move_pos(elf, d)
                    pos ∉ elf_set
                end
                if no_elf
                    new_elves[e] = move_pos(elf, dir3[2])
                    break
                end
            end
        end
    end

    new_elves_cnt::Dict{NTuple{2,Int},Int} = Dict()
    for ne in new_elves
        new_elves_cnt[ne] = get!(new_elves_cnt, ne, 0) + 1
    end
    for e in 1:length(new_elves)
        ne = new_elves[e]
        if new_elves_cnt[ne] > 1
            new_elves[e] = elves[e]
        end
    end
    new_elves, circshift(dirs, -1)
end

function simulate(lines, k)
    dirs::Vector{Vector{NTuple{2,Int}}} = [
        [(-1, -1), (-1, 0), (-1, 1)],
        [(1, -1), (1, 0), (1, 1)],
        [(-1, -1), (0, -1), (1, -1)],
        [(-1, 1), (0, 1), (1, 1)]
    ]
    surrounding::Vector{NTuple{2,Int}} = [
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1), (0, 1),
        (1, -1), (1, 0), (1, 1)
    ]

    elves::Vector{NTuple{2,Int}} = []

    m = length(lines)
    n = length(lines[1])
    for i in 1:m
        for j in 1:n
            if lines[i][j] == '#'
                push!(elves, (i, j))
            end
        end
    end

    for th in 1:k
        old_elves = elves
        elves, dirs = simulate1(elves, dirs, surrounding)
        if old_elves == elves
            return elves, th
        end
    end

    elves, k
end

function day23()
    lines = readlines()

    elves, _ = simulate(lines, 10)

    mini = minimum(p -> p[1], elves)
    maxi = maximum(p -> p[1], elves)
    minj = minimum(p -> p[2], elves)
    maxj = maximum(p -> p[2], elves)

    println((maxj - minj + 1) * (maxi - mini + 1) - length(elves))

    _, th = simulate(lines, 100000)
    println(th)
end

@time day23()
