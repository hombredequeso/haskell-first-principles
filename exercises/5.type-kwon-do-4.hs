munge :: (x -> y)
            -> (y -> (w, z))
            -> x
            -> w
munge xToy yToTup anX = 
    let y = xToy anX in
        let (m,n) = yToTup y in
            m

munge2 xToy yToTup anX = m where 
    y = xToy anX
    (m,n) = yToTup y
