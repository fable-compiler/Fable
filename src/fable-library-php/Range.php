<?php
namespace Range;

function rangeNumber($start, $inc, $end)
{
    while ($start <= $end)
    {
        yield $start;
        $start += $inc;
    }
}

function rangeDouble($start, $inc, $end)
{
    while ($start <= $end)
    {
        yield $start;
        $start += $inc;
    }
}