namespace System

open Global_

type Array() =
    class
    end

type Enum() =
    class
    end

type Exception(message: string) =
    new() = Exception("")

    member _.Message =
        if System.String.IsNullOrEmpty(message) then
            "Specified argument was out of the range of valid values."
        else
            message

type InvalidOperationException(message: string) =
    new() = InvalidOperationException("")

    member _.Message =
        if System.String.IsNullOrEmpty(message) then
            "Operation is not valid due to the current state of the object."
        else
            message

type ArgumentException(message: string, paramName: string) =
    new() = ArgumentException("", "")
    new(message) = ArgumentException(message, "")

    member _.Message =
        let message =
            if System.String.IsNullOrEmpty(message) then
                "Value does not fall within the expected range."
            else
                message

        if System.String.IsNullOrEmpty(paramName) then
            message
        else
            message + " (Parameter '" + paramName + "')"

    member _.ParamName = paramName

type ArgumentOutOfRangeException(paramName: string, message: string) =
    new() = ArgumentOutOfRangeException("", "")
    new(paramName) = ArgumentOutOfRangeException(paramName, "")

    member _.Message =
        let message =
            if System.String.IsNullOrEmpty(message) then
                "Specified argument was out of the range of valid values."
            else
                message

        if System.String.IsNullOrEmpty(paramName) then
            message
        else
            message + " (Parameter '" + paramName + "')"

    member _.ParamName = paramName
