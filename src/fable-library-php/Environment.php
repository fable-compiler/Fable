<?php

namespace Environment;

// System.Environment.GetEnvironmentVariable(name).
// PHP's getenv() returns the value as a string, or false when the variable is
// not set. .NET returns null for a missing variable, so map false to null.
function getEnvironmentVariable($name)
{
    $value = \getenv($name);
    return $value === false ? null : $value;
}
