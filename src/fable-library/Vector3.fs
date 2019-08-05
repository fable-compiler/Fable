namespace System.Numerics

open System

// contains the types Vector3 and Quaternion.
// feel free to add the rest as required


// WIP Notes:
//  need to check if auto-generated equality is OK
//  Vector3:
//    TODO Transform
//  Quaternion:
//    TODO lots of stuff
// for tests, take inspiration from 
//  - https://github.com/dotnet/corefx/blob/master/src/System.Numerics.Vectors/tests/QuaternionTests.cs
//  - https://github.com/dotnet/corefx/blob/master/src/System.Numerics.Vectors/tests/Vector3Tests.cs

[<CustomComparison; CustomEquality>]
type Vector3 =
  struct

    val mutable public X : single
    val mutable public Y : single
    val mutable public Z : single

    new (value : single) = { X = value; Y = value; Z = value }
    new (x : single, y : single, z : single) = { X = x; Y = y; Z = z }

    //member this.Abs() = Vector3(System.Math.Abs(this.X), System.Math.Abs(this.Y), System.Math.Abs(this.Z))

    member this.CopyTo(arr : single array, index : int) =
        arr.[index + 0] <- this.X
        arr.[index + 1] <- this.Y
        arr.[index + 2] <- this.Z

    member this.CopyTo(arr : single array) = this.CopyTo(arr, 0)

    member this.Length() : single =
        sqrt( this.X * this.X + this.Y * this.Y + this.Z * this.Z )

    member this.LengthSquared() : single =
        this.X * this.X + this.Y * this.Y + this.Z * this.Z

    override this.ToString() =
        // note: net-fx passes CultureInfo.CurrentCulture
        this.ToString("G", null)

    member this.ToString(format : string) =
        // note: net-fx passes CultureInfo.CurrentCulture
        this.ToString(format, null)

    member this.ToString(format : string, formatProvider : IFormatProvider) =
        let stringBuilder = new System.Text.StringBuilder()
        // note: net-fx uses System.Globalization.NumberFormatInfo.GetInstance(formatProvider).NumberGroupSeparator
        let numberGroupSeparator = "."
        stringBuilder.Append('<')
          .Append(this.X.ToString(format, formatProvider))
          .Append(numberGroupSeparator)
          .Append(' ')
          .Append(this.Y.ToString(format, formatProvider))
          .Append(numberGroupSeparator)
          .Append(' ')
          .Append(this.Z.ToString(format, formatProvider))
          .Append('>')
          |> ignore
        stringBuilder.ToString()

    member this.Equals(other : Vector3) =
        this.X = other.X &&
        this.Y = other.Y &&
        this.Z = other.Z

    override this.Equals(other : obj) =
        match other with
        | :? Vector3 as vother ->
            this.Equals(vother)
        | _ -> false

    override this.GetHashCode() =
        let o = obj()
        hash(this.X, this.Y, this.Z)


    interface System.IComparable with
        member this.CompareTo(other) =
            match other with
            | null -> failwithf "other is null"
            | :? Vector3 as vother ->
                compare (this.X, this.Y, this.Z) (vother.X, vother.Y, vother.Z)
            | _ -> failwithf "invalid type, expected 'other' to be a Vector, but it is '%O'" (other.GetType())
    

    // --------------------------------
    // Static members of Vector3

    static member One = Vector3(1.f, 1.f, 1.f)

    static member UnitX = Vector3(1.f, 0.f, 0.f)
    static member UnitY = Vector3(0.f, 1.f, 0.f)
    static member UnitZ = Vector3(0.f, 0.f, 1.f)

    static member Zero = Vector3(0.f, 0.f, 0.f)

    static member Abs(value : Vector3) : Vector3 =
        Vector3(System.Math.Abs(value.X), System.Math.Abs(value.Y), System.Math.Abs(value.Z))

    static member Add(left : Vector3, right : Vector3) =
        Vector3(left.X + right.X, left.Y + right.Y, left.Z + right.Z)

    static member Divide(left : Vector3, right : Vector3) =
        Vector3(left.X / right.X, left.Y / right.Y, left.Z / right.Z)

    static member Divide(left : Vector3, right : single) =
        Vector3(left.X / right, left.Y / right, left.Z / right)
        
    static member Multiply(left : Vector3, right : Vector3) =
        Vector3(left.X * right.X, left.Y * right.Y, left.Z * right.Z)
        
    static member Multiply(left : Vector3, right : single) =
        Vector3(left.X * right, left.Y * right, left.Z * right)
        
    static member Multiply(left : single, right : Vector3) =
        Vector3(left * right.X, left * right.Y, left * right.Z)

    static member Subtract(left : Vector3, right : Vector3) =
        Vector3(left.X - right.X, left.Y - right.Y, left.Z - right.Z)

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

    static member Dot(vector1 : Vector3, vector2 : Vector3) : single =
        vector1.X * vector2.X + vector1.Y * vector2.Y + vector1.Z * vector2.Z

    static member Lerp(value1 : Vector3, value2 : Vector3, amount : single) : Vector3 =
        Vector3(value1.X + (value2.X - value1.X) * amount, value1.Y + (value2.Y - value1.Y) * amount, value1.Z + (value2.Z - value1.Z) * amount)

    static member Max(value1 : Vector3, value2 : Vector3) : Vector3 =
        let x = max value1.X value2.X
        let y = max value1.Y value2.Y
        let z = max value1.Z value2.Z
        Vector3(x, y, z)

    static member Min(value1 : Vector3, value2 : Vector3) : Vector3 =
        let x = min value1.X value2.X
        let y = min value1.Y value2.Y
        let z = min value1.Z value2.Z
        Vector3(x, y, z)

    static member Negate(value : Vector3) : Vector3 =
        Vector3.Zero - value

    static member Normalize(value : Vector3) : Vector3 =
        let num = value.X * value.X + value.Y * value.Y + value.Z * value.Z
        let num2 = sqrt (num)
        Vector3(value.X / num2, value.Y / num2, value.Z / num2)

    static member Reflect(vector : Vector3, normal : Vector3) : Vector3 =
        let num = vector.X * normal.X + vector.Y * normal.Y + vector.Z * normal.Z
        let num2 = normal.X * num * 2.f
        let num3 = normal.Y * num * 2.f
        let num4 = normal.Z * num * 2.f
        Vector3(vector.X - num2, vector.Y - num3, vector.Z - num4)

    static member SquareRoot(value : Vector3) : Vector3 =
        let x = sqrt value.X
        let y = sqrt value.Y
        let z = sqrt value.Z
        Vector3(x, y, z)


    // --------------------------------
    // Operators of Vector3
    
    static member (+) (left:Vector3, right:Vector3) : Vector3 =
        Vector3.Add(left, right)

    static member (/) (left:Vector3, right:Vector3) : Vector3 =
        Vector3.Divide(left, right)

    static member (/) (left:Vector3, right:single) : Vector3 =
        Vector3.Divide(left, right)

    static member (==) (left:Vector3, right:Vector3) : bool =
        left.Equals(right)

    static member (!=) (left:Vector3, right:Vector3) : bool =
        not (left.Equals(right))

    static member (*) (left:Vector3, right:Vector3) : Vector3 =
        Vector3.Multiply(left, right)

    static member (*) (left:Vector3, right:single) : Vector3 =
        Vector3.Multiply(left, right)

    static member (*) (left:single, right:Vector3) : Vector3 =
        Vector3.Multiply(left, right)

    //static member (-) (value:Vector3) : Vector3 =
    //    Vector3.Negate(value)

    static member (-) (left:Vector3, right:Vector3) : Vector3 =
        Vector3.Subtract(left, right)
  end
  