<?php

namespace Random;

class Random
{
    #[\ReturnTypeWillChange]
    public function __get(string $propName)
    {
        switch($propName) {
            case "Next0":
                return rand();
            default:
                throw new Error("Attempt to read undefined property $propName");
        };
    }
}

function nonSeeded()
{
    return new Random();
}
