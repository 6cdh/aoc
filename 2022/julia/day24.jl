Pos = NTuple{2,Int}

function next_frame(bpos, bdir, m, n)
    for i in 1:length(bpos)
        p = bpos[i]
        d = bdir[i]
        next = (p[1] + d[1], p[2] + d[2])
        if next[2] == 1
            next = (next[1], n - 1)
        elseif next[2] == n
            next = (next[1], 2)
        elseif next[1] == 1
            next = (m - 1, next[2])
        elseif next[1] == m
            next = (2, next[2])
        end
        bpos[i] = next
    end
    bpos
end

function simulate(bpos, bdir, from, to, m, n)
    queue::Set{Pos} = Set([from])
    steps = 0
    while to ∉ queue
        new_queue::Set{Pos} = Set()
        bpos = next_frame(bpos, bdir, m, n)
        bpos_set::Set{Pos} = Set(bpos)

        for u in queue
            for d in ((-1, 0), (0, -1), (0, 0), (0, 1), (1, 0))
                v = (u[1] + d[1], u[2] + d[2])
                if (2 <= v[1] < m && 2 <= v[2] < n && v ∉ bpos_set) ||
                   (v == from) ||
                   (v == to)
                    push!(new_queue, v)
                end
            end
        end
        steps += 1
        queue = new_queue
    end
    steps, bpos
end

function day24()
    lines = readlines()
    m = length(lines)
    n = length(lines[1])
    bpos::Vector{Pos} = []
    bdir::Vector{Pos} = []

    for i in 1:m
        for j in 1:n
            if lines[i][j] == 'v'
                push!(bpos, (i, j))
                push!(bdir, (1, 0))
            elseif lines[i][j] == '^'
                push!(bpos, (i, j))
                push!(bdir, (-1, 0))
            elseif lines[i][j] == '<'
                push!(bpos, (i, j))
                push!(bdir, (0, -1))
            elseif lines[i][j] == '>'
                push!(bpos, (i, j))
                push!(bdir, (0, 1))
            end
        end
    end

    t1, bpos2 = simulate(bpos, bdir, (1, 2), (m, n - 1), m, n)
    t2, bpos3 = simulate(bpos2, bdir, (m, n - 1), (1, 2), m, n)
    t3, _ = simulate(bpos3, bdir, (1, 2), (m, n - 1), m, n)
    println(t1 + t2 + t3)
end

@time day24()
