function where(heights, c)
    for i in 1:length(heights)
        for j in 1:length(heights[1])
            if heights[i][j] == c
                return (i, j)
            end
        end
    end
    error("No $c exists.")
end

# explore all paths from `start` to every nodes
function bfs(heights, start)
    m = length(heights)
    n = length(heights[1])
    inf = typemax(Int)

    path::Matrix{Int} = fill(inf, m, n)
    q::Vector{NTuple{2,Int}} = [start]
    steps = 0
    while !isempty(q)
        newq::Vector{NTuple{2,Int}} = []
        for node in q
            i, j = node
            for dij in ((0, -1), (0, 1), (-1, 0), (1, 0))
                i1 = i + dij[1]
                j1 = j + dij[2]
                if 1 <= i1 <= m &&
                   1 <= j1 <= n &&
                   path[i1, j1] == inf &&
                   heights[i1][j1] >= heights[i][j] - 1
                    path[i1, j1] = steps + 1
                    push!(newq, (i1, j1))
                end
            end
        end
        q = newq
        steps += 1
    end
    path
end

function day12()
    lines = readlines()
    heights = map(collect, lines)

    beg = where(heights, 'S')
    dst = where(heights, 'E')

    heights[beg[1]][beg[2]] = 'a'
    heights[dst[1]][dst[2]] = 'z'

    path = bfs(heights, dst)

    # part 1
    println(path[beg[1], beg[2]])

    # part 2
    m = length(heights)
    n = length(heights[1])
    best = typemax(Int)
    for i in 1:m
        for j in 1:n
            if heights[i][j] == 'a'
                best = min(best, path[i, j])
            end
        end
    end
    println(best)
end

@time day12()
