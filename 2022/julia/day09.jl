using Match

function adjust_tail(prev, cur)
    Δx = prev[1] - cur[1]
    Δy = prev[2] - cur[2]
    if abs(Δx) <= 1 && abs(Δy) <= 1
        cur
    else
        sx = sign(Δx)
        sy = sign(Δy)
        (cur[1] + sx, cur[2] + sy)
    end
end

function move(ropes, Δ, k, visited)
    n = length(ropes)
    for _ in 1:k
        ropes[1] = (ropes[1][1] + Δ[1], ropes[1][2] + Δ[2])
        for i in 2:n
            ropes[i] = adjust_tail(ropes[i-1], ropes[i])
        end
        push!(visited, ropes[n])
    end
    ropes
end

function emulate(lines, n)
    visited::Set{NTuple{2,Int}} = Set([(0, 0)])
    ropes = fill((0, 0), n)
    for line in lines
        words = split(line)
        dir, steps = words[1], parse(Int, words[2])

        ropes =
            @match dir begin
                "L" => move(ropes, (-1, 0), steps, visited)
                "R" => move(ropes, (1, 0), steps, visited)
                "U" => move(ropes, (0, 1), steps, visited)
                "D" => move(ropes, (0, -1), steps, visited)
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
