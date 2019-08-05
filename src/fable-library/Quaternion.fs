namespace System.Numerics

open System

  
type Quaternion =
  struct

    val mutable public X : single
    val mutable public Y : single
    val mutable public Z : single
    val mutable public W : single

    new (x : single, y : single, z : single, w : single) = { X = x; Y = y; Z = z; W = w }
    new(vectorPart : Vector3, scalarPart : single) =
        Quaternion(vectorPart.X, vectorPart.Y, vectorPart.Z, scalarPart)

    member this.IsIdentity =
        this.X = 0.f &&
        this.Y = 0.f &&
        this.Z = 0.f &&
        this.W = 1.f

    member this.Length() : single =
        let ls = this.LengthSquared()
        single ( System.Math.Sqrt(float ls) )

    member this.LengthSquared() : single =
        this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W
        

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

  end