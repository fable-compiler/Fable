<?php
namespace FSharpArray;

function ofList($list)
{
    return \FSharpList\toArray($list);
} 

function toList($array)
{
    return \FSharpList\ofArray($array);
}

function ofSeq($seq)
{
    if (is_array($seq))
    {
        return $seq;
    }
    else
    {
        $array = [];
        foreach($seq as $item)
        {
            $array[] = $item;
        }
        return $array;
    }
}

function filter($filter,$array)
{
    $result = [];
    foreach($array as $item)
    {
        if ($filter($item))
        {
            $result[] = $item;
        }
    }
    return $result;
}

function findIndex($predicate, $array)
{
    $index = 0;
    foreach ($array as $item)
    {
        if ($predicate($item))
        {
            return $index;
        }
        $index++;
    }

    return -1;
}