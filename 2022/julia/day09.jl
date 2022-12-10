using Match

function adjust_knot(prev, cur)
    Δx = prev[1] - cur[1]
    Δy = prev[2] - cur[2]
    if abs(Δx) <= 1 && abs(Δy) <= 1
        cur
    else
        (cur[1] + sign(Δx), cur[2] + sign(Δy))
    end
end

function move!(rope, Δ, k, visited)
    n = length(rope)
    for _ in 1:k
        rope[1] = (rope[1][1] + Δ[1], rope[1][2] + Δ[2])
        for i in 2:n
            rope[i] = adjust_knot(rope[i-1], rope[i])
        end
        push!(visited, rope[n])
    end
end

function emulate(lines, n)
    visited::Set{NTuple{2,Int}} = Set([(0, 0)])
    rope = fill((0, 0), n)
    for line in lines
        words = split(line)
        dir, steps = words[1], parse(Int, words[2])

        @match dir begin
            "L" => move!(rope, (-1, 0), steps, visited)
            "R" => move!(rope, (1, 0), steps, visited)
            "U" => move!(rope, (0, 1), steps, visited)
            "D" => move!(rope, (0, -1), steps, visited)
        end
    end
    length(visited)
end

function day09()
    lines = readlines()

    println(emulate(lines, 2))
    println(emulate(lines, 10))
end

@time day09()
