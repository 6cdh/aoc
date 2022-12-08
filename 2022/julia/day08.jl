function scoreof(tall, d)
    cnt = findfirst(x -> x >= tall, d)
    if isnothing(cnt)
        length(d)
    else
        cnt
    end
end

function day08()
    lines = readlines()

    m = length(lines)
    n = length(lines[1])

    left(i, j) = lines[i][j-1:-1:1]
    right(i, j) = lines[i][j+1:n]
    up(i, j) = map(l -> l[j], lines[i-1:-1:1])
    down(i, j) = map(l -> l[j], lines[i+1:m])
    look(i, j) = [left(i, j), right(i, j), up(i, j), down(i, j)]

    cnt = 0
    max_score = 0
    for i in 1:m
        for j in 1:n
            fields = look(i, j)
            tall = lines[i][j]

            score = prod(map(d -> scoreof(tall, d), fields))
            max_score = max(max_score, score)

            if i == 1 || i == m || j == 1 || j == n ||
               any(d -> tall > maximum(d), fields)
                cnt += 1
            end
        end
    end
    println(cnt)
    println(max_score)
end

@time day08()
