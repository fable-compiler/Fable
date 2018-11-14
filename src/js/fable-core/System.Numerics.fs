namespace System.Numerics

// contains the types Vector3 and Quaternion.
// feel free to add the rest as required



[<Struct>]
type Vector3(x : single, y : single, z : single) =

    new (value : single) = Vector3(value, value, value)

    member __.X = x
    member __.Y = y
    member __.Z = z

    member __.Abs() = Vector3(System.Math.Abs(x), System.Math.Abs(y), System.Math.Abs(z))

    member __.CopyTo(arr : single array, index : int) =
        arr.[index + 0] <- x
        arr.[index + 1] <- y
        arr.[index + 2] <- z

    member __.CopyTo(arr : single array) = __.CopyTo(arr, 0)

    //member __.Equals(other : Vector3) =
    //    x = other.X &&
    //    y = other.Y &&
    //    z = other.Z

    //override this.Equals(other : obj) =
    //    match other with
    //    | :? Vector3 as vother ->
    //        this.Equals(vother)
    //    | _ -> false

    //override __.GetHashCode() =
    //    hash(x, y, z)
    
    // --------------------------------
    // Static members of Vector3

    static member One = Vector3(1.f, 1.f, 1.f)

    static member UnitX = Vector3(1.f, 0.f, 0.f)
    static member UnitY = Vector3(0.f, 1.f, 0.f)
    static member UnitZ = Vector3(0.f, 0.f, 1.f)

    static member Zero = Vector3(0.f, 0.f, 0.f)

    static member Add(left : Vector3, right : Vector3) =
        Vector3(left.X + right.X, left.Y + right.Y, left.Z + right.Z)

    static member Divide(left : Vector3, right : Vector3) =
        Vector3(left.X / right.X, left.Y / right.Y, left.Z / right.Z)

    static member Divide(left : Vector3, right : single) =
        Vector3(left.X / right, left.Y / right, left.Z / right)

    static member Clamp(value : Vector3, min : Vector3, max : Vector3) =
        let inline clamp v min max =
            if v < min then min
            else if v > max then max
            else v
        Vector3(clamp value.X min.X max.X,
                clamp value.Y min.Y max.Y,
                clamp value.Z min.Z max.Z)

    static member Cross(vector1 : Vector3, vector2 : Vector3) =
        Vector3(vector1.Y * vector2.Z - vector1.Z * vector2.Y,
                vector1.Z * vector2.X - vector1.X * vector2.Z,
                vector1.X * vector2.Y - vector1.Y * vector2.X)

    static member Distance(value1 : Vector3, value2 : Vector3) : single =
        let num2 = value1.X - value2.X
        let num3 = value1.Y - value2.Y
        let num4 = value1.Z - value2.Z
        let num5 = num2 * num2 + num3 * num3 + num4 * num4;
        single ( System.Math.Sqrt(float num5) )

    static member DistanceSquared(value1 : Vector3, value2 : Vector3) : single =
        let num2 = value1.X - value2.X
        let num3 = value1.Y - value2.Y
        let num4 = value1.Z - value2.Z
        let num5 = num2 * num2 + num3 * num3 + num4 * num4;
        num5

    // --------------------------------
    // Operators of Vector3
    
    static member (+) (left:Vector3, right:Vector3) : Vector3 =
        Vector3.Add(left, right)

    static member (/) (left:Vector3, right:Vector3) : Vector3 =
        Vector3.Divide(left, right)

    static member (/) (left:Vector3, right:single) : Vector3 =
        Vector3.Divide(left, right)


[<Struct>]
type Quaternion(x : single, y : single, z : single, w : single) =
    
    new(vectorPart : Vector3, scalarPart : single) =
        Quaternion(vectorPart.X, vectorPart.Y, vectorPart.Z, scalarPart)

    member __.X = x
    member __.Y = y
    member __.Z = z
    member __.W = w

    member __.IsIdentity =
        x = 0.f &&
        y = 0.f &&
        z = 0.f &&
        w = 1.f

    member this.Length() : single =
        let ls = this.LengthSquared()
        single ( System.Math.Sqrt(float ls) )

    member __.LengthSquared() : single =
        x * x + y * y + z * z + w * w
        
    // --------------------------------
    // static members of Quaternion

    static member Identity = Quaternion(0.f, 0.f, 0.f, 1.f)

    static member Add(value1 : Quaternion, value2 : Quaternion) =
        Quaternion(
            value1.X + value2.X,
            value1.Y + value2.Y,
            value1.Z + value2.Z,
            value1.W + value2.W
        )

    static member Concatenate(value1 : Quaternion, value2 : Quaternion) =
        let x = value2.X;
        let y = value2.Y;
        let z = value2.Z;
        let w = value2.W;
        let x2 = value1.X;
        let y2 = value1.Y;
        let z2 = value1.Z;
        let w2 = value1.W;
        let num = y * z2 - z * y2;
        let num2 = z * x2 - x * z2;
        let num3 = x * y2 - y * x2;
        let num4 = x * x2 + y * y2 + z * z2;
        Quaternion(
            x * w2 + x2 * w + num,
            y * w2 + y2 * w + num2,
            z * w2 + z2 * w + num3,
            w * w2 - num4
        )

    static member Conjugate(value : Quaternion) =
        Quaternion(
            0.f - value.X,
            0.f - value.Y,
            0.f - value.Z,
            value.W
        )

    static member Divide(value1 : Quaternion, value2 : Quaternion) =
        let x = value1.X;
        let y = value1.Y;
        let z = value1.Z;
        let w = value1.W;
        let num = value2.X * value2.X + value2.Y * value2.Y + value2.Z * value2.Z + value2.W * value2.W;
        let num2 = 1.f / num;
        let num3 = (0.f - value2.X) * num2;
        let num4 = (0.f - value2.Y) * num2;
        let num5 = (0.f - value2.Z) * num2;
        let num6 = value2.W * num2;
        let num7 = y * num5 - z * num4;
        let num8 = z * num3 - x * num5;
        let num9 = x * num4 - y * num3;
        let num10 = x * num3 + y * num4 + z * num5;
        Quaternion(
            x * num6 + num3 * w + num7,
            y * num6 + num4 * w + num8,
            z * num6 + num5 * w + num9,
            w * num6 - num10
        )

    static member Dot(quaternion1 : Quaternion, quaternion2 : Quaternion) : single =
        quaternion1.X * quaternion2.X + quaternion1.Y * quaternion2.Y + quaternion1.Z * quaternion2.Z + quaternion1.W * quaternion2.W

    static member Inverse(value : Quaternion) : Quaternion =
        let num = value.X * value.X + value.Y * value.Y + value.Z * value.Z + value.W * value.W;
        let num2 = 1.f / num;
        Quaternion(
            (0.f - value.X) * num2,
            (0.f - value.Y) * num2,
            (0.f - value.Z) * num2,
            value.W * num2
        )

    static member Multiply(value1 : Quaternion, value2 : Quaternion) : Quaternion =
        let x = value1.X;
        let y = value1.Y;
        let z = value1.Z;
        let w = value1.W;
        let x2 = value2.X;
        let y2 = value2.Y;
        let z2 = value2.Z;
        let w2 = value2.W;
        let num = y * z2 - z * y2;
        let num2 = z * x2 - x * z2;
        let num3 = x * y2 - y * x2;
        let num4 = x * x2 + y * y2 + z * z2;
        Quaternion(
            x * w2 + x2 * w + num,
            y * w2 + y2 * w + num2,
            z * w2 + z2 * w + num3,
            w * w2 - num4
        )

    static member Multiply(value1 : Quaternion, value2 : single) : Quaternion =
        Quaternion(
            value1.X * value2,
            value1.Y * value2,
            value1.Z * value2,
            value1.W * value2
        )

    // --------------------------------
    // Operators of Quaternion
    
    static member (+) (left:Quaternion, right:Quaternion) : Quaternion =
        Quaternion.Add(left, right)
    
    static member (/) (left:Quaternion, right:Quaternion) : Quaternion =
        Quaternion.Divide(left, right)
    
    static member (*) (left:Quaternion, right:Quaternion) : Quaternion =
        Quaternion.Multiply(left, right)
    
    static member (*) (left:Quaternion, right:single) : Quaternion =
        Quaternion.Multiply(left, right)