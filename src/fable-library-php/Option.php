<?php
namespace Option;

function  defaultArg($opt, $val)
{
    return is_null($opt) ? $val : $opt;
}