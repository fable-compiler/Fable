<?php
namespace Seq;
use \IteratorAggregate;

function singleton($v) {
    return [$v];
} 

function _empty() {
    static $empty = NULL;
    if (is_null($empty)) {
        $empty = [];
    }
    return $empty;
}

function toList($seq) {
    return \FSharpList\ofSeq($seq);
}

function toArray($seq) {
    return \FSharpArray\ofSeq($seq);
}

function filter($filter, $seq)
{
    foreach($seq as $item)
    { 
        if ($filter($item))
            yield $item;
    }
}
function map($projection, $seq)
{
    foreach($seq as $item)
    { 
        yield $projection($item);
    }
}

function choose($projection, $seq)
{
    foreach($seq as $item)
    { 
        $value = $projection($item);
        if (!is_null($value))
        {
            yield $value;
        }
    }
}


function collect($projection, $seq)
{
    if (is_null($seq))
        throw new \Exception("Seq should not be null");

    foreach($seq as $item)
    {
        foreach($projection($item) as $i)
        {
            yield $i;
        }
    }
}

function contains($value, $seq)
{
    foreach($seq as $item)
    {
        if ($item == $value)
            return true;
    }

    return false;

}

function tryFindIndex($test, $seq)
{
    $index = 0;
    foreach($seq as $item)
    {
        if ($test($item))
            return $index;

        $index++;
    }
    return NULL;
}

function exists($test, $seq)
{
    foreach($seq as $item)
    {
        if ($test($item))
            return true;

    }
    return false; 
}

function delay($f) 
{
    return new DelayedSeq($f);
}


function append($x,$y)
{
    foreach($x as $i)
    {
        yield $i;
    }
    foreach($y as $i)
    {
        yield $i;
    }
}


function maxBy($property, $seq, $comparerArray)
{
    $max = NULL;
    $maxVal = NULL;
    $comparer = $comparerArray['Compare'];
    foreach($seq as $item)
    {
        $prop = $property($item);

        if (is_null($max) || $comparer($prop,$max) > 0)
        {
            $max = $prop;
            $maxVal = $item;
        }
    }

    return $maxVal;
}

function max($seq, $comparerArray)
{
    $max = NULL;
    $comparer = $comparerArray['Compare'];
    foreach($seq as $item)
    {
        if (is_null($max) || $comparer($item,$max) > 0)
        {
            $max = $item;
        }
    }

    return $max;
}

    

function fold($aggregator, $state, $seq)
{
    foreach ($seq as $item)
    {
        $state = $aggregator($state,$item);
    }
    return $state;
}

function iterate($f, $seq)
{
    foreach($seq as $item)
    {
        $f($item);
    }
}



class DelayedSeq implements IteratorAggregate {
    public $f;
    function __construct($f) {
        $this->f = $f;
    }

    public function getIterator() {
        $f = $this->f;
        $seq = $f(NULL);
        foreach($seq as $item)
            yield $item;
    } 
    
}
