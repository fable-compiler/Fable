<?php
namespace Util;

require_once "FSharp.Core.php";
require_once "Range.php";

$equals = function ($x,$y) { return $x == $y; };

function equals($x,$y) { 
    if ( $x instanceof IComparable)
        return $x->CompareTo($y) == 0;
    
    return $x == $y; }

function max($comparer, $x, $y) {
    return $comparer($x,$y) >= 0 ? $x : $y;
}
function min($comparer, $x, $y) {
    return $comparer($x,$y) <= 0 ? $x : $y;
}


function randomNext($min,$max)
{
    return bga_rand ($min , $max );
}


function comparePrimitives($x,$y) 
{ return $x == $y ? 0 : ($x > $y ? 1 : -1); }

function compareArrays ($x,$y) { 
    $i = 0;
    $xl = count($x);
    $yl = count($y);
    while(true)
    {
        if ($i == $xl && $i == $yl)
            return 0;
        if ($i == $xl)
            return -1;
        if ($i == $yl)
            return 1;

        $c = compare($x[$i],$y[$i]);
        if ($c !== 0)
            return $c;
        $i++;
    }
}

function compare($x,$y)
{
    if (is_array($x))
    {
        if (is_array($y))
            return compareArrays($x,$y);
        else
            return 1;
    }
    elseif ($x instanceof IComparable )
    {
        return $x->CompareTo($y);
    }
    {
        if (is_array($y))
            return -1;
        else
            return comparePrimitives($x,$y);
    }

    
}

function stringHash($s) {
    $i = 0;
    $h = 5381;
    $len = strlen($s);
    while ($i < $len) {
        $h = ($h * 33) ^ $s[$i++];
    }
    return $h;
}

function numberHash($x) {
    return $x * 2654435761;
}


function arrayHash($x) {
    $len = count($x);
    $hashes = [];
    for ($i = 0; $i < $len; $i++) {
        $hashes[$i] = structuralHash($x[$i]);
    }
    return combineHashCodes($hashes);
}
function combineHashCodes($hashes) {
    if (count($hashes) === 0) {
        return 0;
    }
    $hash = 0;
    foreach ($hashes as $value) {
        $hash = (($hash << 5) + $hash) ^ $value;
    }
    return $hash;
}

function structuralHash($x) {
    if (is_null($x)) {
        return 0;
    }
    switch (gettype($x)) {
        case "boolean":
            return $x ? 1 : 0;
        case "integer":
            return numberHash($x);
        case "double":
            return numberHash($x);
        case "string":
            return stringHash($x);
        case "array":
            return arrayHash($x);
        default: {
            if ($x instanceof IEquatable) {
                return $x->GetHashCode();
            }
            // else if ($x instanceof Date) {
            //     return dateHash($x);
            // }
            $vars = get_object_vars ($x);
            if (count($vars) != 0)
            {
                $vals = [];
                foreach ($vars as $value) {
                    $vals[] = structuralHash($value);
                }
                return combineHashCodes($vals);
            }

            else {
                // Classes don't implement GetHashCode by default, but must use identity hashing
                return stringHash(spl_object_hash ($x));
            }
        }
    }
}
function safeHash($x)
{
    if ($x === NULL)
    {
        return 0;
    }
    if ($x instanceof IEquatable)
    {
        return $x->GetHashCode();
    }
    
    return stringHash(spl_object_hash ($x));
}

function getEnumerator($enum) {
    return new Enumerator($enum->getIterator());
}

class Enumerator {
    public $iter;
    public $started;
    function __construct($iter)
    {
        $this->iter = $iter;
        $this->started = false;
    }

    public function MoveNext() {
        if (!$this->started)
        {
            $this->started = true; 
            return $this->iter->valid();
        }
        $this->iter->next();
        return $this->iter->valid();
    }

    public function get_Current() {
        return $this->iter->current();
    }
}
