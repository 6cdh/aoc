function adjacent_cubes(cube)
    map(dir -> cube + dir, [[0, 0, 1], [0, 0, -1], [0, 1, 0], [0, -1, 0], [1, 0, 0], [-1, 0, 0]])
end

function dfs(cube, mins, maxs, cubes, surface)
    if mins[1] <= cube[1] <= maxs[1] &&
       mins[2] <= cube[2] <= maxs[2] &&
       mins[3] <= cube[3] <= maxs[3] &&
       cube ∉ cubes && cube ∉ surface
        push!(surface, cube)
        for new_cube in adjacent_cubes(cube)
            dfs(new_cube, mins, maxs, cubes, surface)
        end
    end
end

function day18()
    lines = readlines()

    cubes::Set{Vector{Int}} = Set()
    for line in lines
        x, y, z = parse.(Int, line |> x -> split(x, ","))
        push!(cubes, [x, y, z])
    end

    adjs::Vector{Vector{Int}} = []
    for c in cubes
        append!(adjs, adjacent_cubes(c))
    end
    println(count(adj -> adj ∉ cubes, adjs))

    # don't use `zip(adjs...)`, it's much slower
    xs = map(adj -> adj[1], adjs)
    ys = map(adj -> adj[2], adjs)
    zs = map(adj -> adj[3], adjs)
    mins::Vector{Int} = [minimum(xs), minimum(ys), minimum(zs)]
    maxs::Vector{Int} = [maximum(xs), maximum(ys), maximum(zs)]
    exterior_surface::Set{Vector{Int}} = Set()

    dfs(mins, mins, maxs, cubes, exterior_surface)
    println(count(adj -> adj ∈ exterior_surface, adjs))
end

@time day18()
