<?php

namespace Random;

class Random
{
    public function __get(string $propName): mixed
    {
        return match ($propName) {
            'Next0' => rand(),
            default => throw new Error("Attempt to read undefined property $propName"),
        };
    }
}

function nonSeeded()
{
    return new Random();
}
