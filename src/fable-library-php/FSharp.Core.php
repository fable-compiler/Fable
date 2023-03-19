<?php

interface IComparable {
    public function CompareTo($Other);
}

interface FSharpUnion {
    public static function get_FSharpCase();
}

interface IDisposable {
    public function Dispose();
}

function void($x) {}

class Result {

}

class Result_Ok extends Result {
    public $ResultValue;
    function __construct($value)
    {
        $this->ResultValue = $value;
    }

    public function get_Tag() {
        return 0;
    }
}

class Result_Error extends Result {
    public $ErrorValue;
    function __construct($value)
    {
        $this->ErrorValue = $value;
    }
    public function get_Tag() {
        return 1;
    }
}
