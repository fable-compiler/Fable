<?php
namespace FSharpList;
require_once "Util.php";

use \IComparable, \IteratorAggregate ;

abstract class FSharpList implements IteratorAggregate, IComparable {
    
    abstract function isEmpty();


    function __debugInfo() {
        return toArray($this);
    }

    public function CompareTo($other)
    {
        if ($this instanceof Nil)
            return $other instanceof Nil ? 0 : -1;
        if ($other instanceof Nil)
            return 1;
        $c = \Util\compare($this->value,$other->value);
        if ($c != 0)
            return $c;
        return $this->next->CompareTo($other->next);
    } 

    public function getIterator() {
        $list = $this;
        while ($list instanceof Cons)
        {
            yield $list->value;
            $list = $list->next;
        }
    } 
}



class Nil extends FSharpList {
    public function __construct() {}
    
    function isEmpty() { return true; }
}

$GLOBALS['NIL'] = new Nil();

class Cons extends FSharpList {
    public $value;
    public $next;

    function __construct($value, FSharpList $next = NULL) {
        $this->value = $value;
        $this->next = $next;
    }

    function isEmpty() { return false; }
}

function cons($hd, $tl) {
    return new Cons($hd, $tl);
}

function _empty() {
    return $GLOBALS['NIL'];
}

function isEmpty($list) {
    return $list->isEmpty();
}

function length(?FSharpList $list) {
    $len = 0;
    while($list instanceof Cons)
    {
        $len++;
        $list = $list->next;
    }

    return $len;
}

function ofArray($a) {
    $list = NULL;
    $p = &$list;
    foreach ($a as $item) {
        
        $p = new Cons($item, $list);
        $p = &$p->next;
    }
    $p = $GLOBALS['NIL'];
    return $list;
}

function toArray($list) {
    $array = [];
    while($list instanceof Cons)
    {
        $array[] = $list->value;
        $list = $list->next;
    }
    return $array;
}

function ofSeq($seq) {
    $list = NULL;
    $p = &$list;
    foreach ($seq as $item) {
        
        $p = new Cons($item, $list);
        $p = &$p->next;
    }
    $p = $GLOBALS['NIL'];
    return $list;
}

function toSeq($list) {
    return $list;
}
    
function contains($item, $list, $comparer)
{
    $eq = $comparer['Equals'];
    while($list instanceof Cons)
    {
        if ($eq($item, $list->value))
        {
            return true;
        }

        $list = $list->next;
    }
    return false;
}

function tryFindIndex($filter, $list)
{
    $i = 0;
    while ($list instanceof Cons)
    {   
        $v = $filter($list->value);
        if ($v)
        {
            return $i;
        }
        $i++;
        $list = $list->next;
    }
    return NULL;
}

function tryFind($filter, $list)
{
    while ($list instanceof Cons)
    {   
        $v = $filter($list->value);
        if ($v)
        {
            return $list->value;
        }
        $list = $list->next;
    }
    return NULL;
}

function skip($count, $list)
{
    while ($count-- > 0 && $list instanceof Cons)
    {   
        $list = $list->next;
    }
    return $list;
}

function item($count, $list)
{
    while ($count-- >= 0 && $list instanceof Cons) {
        $list = $list->next;
    }
    return $list->value;
}

function reverse($list)
{
    $result = $GLOBALS['NIL'];
    while ($list instanceof Cons)
    {
        $result = new Cons($list->value, $result);
        $list = $list->next;
    }

    return $result;
}


function splitAt($i, $list)
{
    $left = NULL;
    $p = &$left;

    while ($i-- > 0 && $list instanceof Cons)
    {
        $p = new Cons($list->value, $left);
        $p = &$p->next;
        $list = $list->next;
    }
    $p = $GLOBALS['NIL'];

    return [ $left, $list];
}

function append($left,$right)
{
    $result = NULL;
    $p = &$result;

    while ($left instanceof Cons)
    {
        $p = new Cons($left->value, NULL);
        $p = &$p->next;
        $left = $left->next;
    }
    $p = $right;
    return $result;
}

function head($list)
{
    return $list->value;
}

function tail($list)
{
    return $list->next;
}

function last($list)
{
    $value = NULL;
    while($list instanceof Cons)
    {
        $value = $list->value;
        $list = $list->next;
    }

    return $value;
}

function truncate($count, $list)
{
    $lst = NULL;
    $p = &$lst;
    while ($list instanceof Cons && $count-- > 0)
    {
        $p = new Cons($list->value, NULL);
        $p = &$p->next;    
        $list = $list->next;
    }
    $p = $GLOBALS['NIL'];
    return $lst;
}

function map($projection, $list)
{
    $lst = NULL;
    $p = &$lst;
    while ($list instanceof Cons)
    {
        $p = new Cons($projection($list->value), NULL);
        $p = &$p->next;    
        $list = $list->next;
    }
    $p = $GLOBALS['NIL'];
    return $lst;
}

function zip($list1, $list2)
{
    $lst = null;
    $p = &$lst;
    while ($list1 instanceof Cons && $list2 instanceof Cons) {
        $p = new Cons([$list1->value, $list2->value], null);
        $p = &$p->next;
        $list1 = $list1->next;
        $list2 = $list2->next;
    }
    $p = $GLOBALS['NIL'];
    return $lst;
}

function choose($projection, $list)
{
    $lst = NULL;
    $p = &$lst;
    while ($list instanceof Cons)
    {
        $v = $projection($list->value);
        if (!is_null($v))
        {
            $p = new Cons($v, NULL);
            $p = &$p->next;
        }
        $list = $list->next;
    }
    $p = $GLOBALS['NIL'];
    return $lst;
}

function filter($predicate, $list)
{
    $lst = NULL;
    $p = &$lst;
    while ($list instanceof Cons)
    {
        if ($predicate($list->value))
        {
            $p = new Cons($list->value, NULL);
            $p = &$p->next;
        }
        $list = $list->next;
    }
    $p = $GLOBALS['NIL'];
    return $lst;
}

function fold($aggregator, $state, $list)
{
    while ($list instanceof Cons)
    {
        $state = $aggregator($state,$list->value);
        $list = $list->next;
    }
    return $state;
}

function mapFold($aggregator, $state, $list)
{
    $lst = NULL;
    $p = &$lst;
    while ($list instanceof Cons)
    {
        $v = $aggregator($state,$list->value);

        $p = new Cons($v[0], NULL);
        $p = &$p->next;
        $state = $v[1];
        $list = $list->next;
    }
    $p = $GLOBALS['NIL'];
    return [$lst,$state];
}

function sortBy($projection, $list)
{
    $array = [];
    while ($list instanceof Cons)
    {
        $array[] = [ $list->value, $projection($list->value) ];
        $list = $list->next;
    }
    usort($array, function($x,$y) { return \Util\compare($x[1],$y[1]); } );

    $list = NULL;
    $p = &$list;
    foreach ($array as $item) {
        
        $p = new Cons($item[0], $list);
        $p = &$p->next;
    }
    $p = $GLOBALS['NIL'];
    return $list;

    return ofArray($array);
}


function collect($projection, $list)
{
    return ofSeq(\Seq\collect($projection, $list));
}

function groupBy($property, $list)
{
    $comparer = [ 'Compare' => function ($x, $y) {     return \Util\compare($x, $y); } ];
    $map = \Map\_empty($comparer);

    while($list instanceof Cons)
    {
        $key = $property($list->value);
        $items = \Map\tryFind($key, $map) ?? [];
        array_push($items, $list->value);
        $map = \Map\add($key, $items, $map);
        $list = $list->next;
    }

    $lst = NULL;
    $p = &$lst; 

    foreach (\Map\toSeq($map) as $kv)
    {
        $p = new Cons([ $kv[0], ofArray($kv[1])], NULL);
        $p = &$p->next;
    }

    $p = $GLOBALS['NIL'];
    return $lst;

}

function chunkBySize($size, $list)
{
    $lst = NULL;
    $p = &$lst;

    $chunk = NULL;
    $pc = &$chunk;

    $c = $size;
    while ($list instanceof Cons)
    {

        $pc = new Cons($list->value, NULL);
        $pc = &$pc->next;
        $list = $list->next;

        if (--$c == 0)
        {
            $pc = $GLOBALS['NIL'];
            $p = new Cons($chunk, NULL);
            $p = &$p->next;
            $chunk = NULL;
            $pc = &$chunk;
            $c = $size;
        }

    }

    if ($chunk instanceof Cons)
    {
        $pc = $GLOBALS['NIL'];
        $p = new Cons($chunk, NULL);
        $p = &$p->next;
    }

    $p = $GLOBALS['NIL'];
    return $lst;
}

function sumBy($property, $list)
{
    $sum = 0;
    while ($list instanceof Cons)
    {
        $sum += $property($list->value);
        $list = $list->next;
    }
    return $sum;
}


function maxBy($property, $list, $comparerArray)
{
    $max = NULL;
    $maxVal = NULL;
    $comparer = $comparerArray['Compare'];
    while($list instanceof Cons)
    {
        $prop = $property($list->value);

        if (is_null($max) || $comparer($prop,$max) > 0)
        {
            $max = $prop;
            $maxVal = $list->value;
        }

        $list = $list->next;
    }

    return $maxVal;
}

function forAll($predicate, $list)
{
    while($list instanceof Cons)
    {
        if (!$predicate($list->value))
            return false;
        $list = $list->next;
    }

    return true;
}

function exists($predicate, $list)
{
    while($list instanceof Cons)
    {
        if ($predicate($list->value))
            return true;
        $list = $list->next;
    }
    return false;
}

function map2($projection, $list1, $list2)
{
    $lst = NULL;
    $p = &$lst;
    while($list1 instanceof Cons and $list2 instanceof Cons)
    {
        $p = new Cons($projection($list1->value, $list2->value), NULL); 
        $p = &$p->next;
        $list1 = $list1->next;
        $list2 = $list2->next;
    }

    $p = $GLOBALS['NIL'];
    return $lst;
}


