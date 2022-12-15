function dist(pos1, pos2)
    abs(pos1[1] - pos2[1]) + abs(pos1[2] - pos2[2])
end

function read_input()
    lines = readlines()

    beacons::Set{Tuple{Int,Int}} = Set()
    sensors::Vector{NTuple{3,Int}} = []

    for line in lines
        m = collect(eachmatch(r"[-0-9]+", line))
        sx = parse(Int, m[1].match)
        sy = parse(Int, m[2].match)
        beacon = (parse(Int, m[3].match), parse(Int, m[4].match))
        sd = dist((sx, sy), beacon)
        push!(beacons, beacon)
        push!(sensors, (sx, sy, sd))
    end

    sensors, beacons
end

function puzzle1(sensors, beacons)
    lx = minimum(map(s -> s[1] - s[3], sensors))
    rx = maximum(map(s -> s[1] + s[3], sensors))

    y = 2000000
    count(lx:rx) do x
        any(sensors) do s
            sx, sy, sd = s
            (x, y) ∉ beacons && dist((x, y), (sx, sy)) <= sd
        end
    end
end

function puzzle2(sensors)
    for s in sensors
        sx, sy, sd = s
        for x in sx-sd-1:sx+sd+1
            dy = if x <= sx
                x - (sx - sd - 1)
            else
                sd + 1 - (x - sx)
            end
            for y in (sy - dy, sy + dy)
                if all(sensors) do s2
                    0 <= x <= 4000000 &&
                        0 <= y <= 4000000 &&
                        dist((x, y), (s2[1], s2[2])) > s2[3]
                end
                    return 4000000 * x + y
                end
            end
        end
    end
end

function day15()
    sensors, beacons = read_input()

    println(puzzle1(sensors, beacons))
    println(puzzle2(sensors))
end

@time day15()
