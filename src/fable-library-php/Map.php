<?php
namespace Map;
use \IteratorAggregate,\Exception,\FSharpList\Cons;

class MapOne extends MapTree {
    public $key;
    public $value;
    function __construct($key, $value)
    {
        $this->key = $key;
        $this->value = $value;
    }
} 

class MapNode extends MapOne {
    public $left;
    public $right;
    public $height;

    function __construct($key, $value, $left, $right, $height)
    {
        parent::__construct($key, $value);
        $this->left = $left;
        $this->right = $right;
        $this->height = $height;
    }
}

class MapTree {
    static function sizeAux($acc,$m)
    { 
        if (is_null($m))
            return $acc;
        if ($m instanceof MapNode)
                return MapTree::sizeAux(MapTree::sizeAux($acc+1, $m->left), $m->right); 
        return $acc + 1;
    }

    static function size($x) {
        return MapTree::sizeAux(0, $x);
    }
    
    static function height ($m)
    { 
        if (is_null($m))
                return 0;
        if ($m instanceof MapNode)
                return $m->height;
        return 1;
    }

    static function isEmpty($m)
    { 
        return is_null($m); 
    }

    static function mk($l, $k, $v, $r)
    { 
        if(is_null($l) && is_null($r))
            return new MapOne ($k, $v);
        $hl = MapTree::height($l); 
        $hr = MapTree::height($r); 
        $m = $hl < $hr ? $hr : $hl; 
        return new MapNode ($k, $v, $l, $r, $m+1);
    }

    static function rebalance($t1, $k, $v, $t2)
    {
        $t1h = MapTree::height($t1); 
        $t2h = MapTree::height($t2); 
        if ($t2h > $t1h + 2)
        {
            // right is heavier than left
            if ($t2 instanceof MapNode)
            { 
                $t2k = $t2->key;
                $t2v = $t2->value;
                $t2l = $t2->left;
                $t2r = $t2->right; 
                // one of the nodes must have height > height t1 + 1 
                if (MapTree::height($t2l) > $t1h + 1)
                {
                    // balance left: combination 
                    if (!($t2l instanceof MapNode))
                        throw new Error("rebalance");
                    $t2lk = $t2l->key;
                    $t2lv = $t2l->value;
                    $t2ll = $t2l->left;
                    $t2lr = $t2l->right;
                    return MapTree::mk(MapTree::mk($t1, $k, $v, $t2ll), $t2lk, $t2lv, MapTree::mk($t2lr, $t2k, $t2v, $t2r)); 
                }
                else
                { 
                    // rotate left 
                    return MapTree::mk(MapTree::mk($t1, $k, $v, $t2l), $t2k, $t2v, $t2r);
                }
            }
            else
            {
                throw new Error("rebalance");
            }
        }
        else
        {
            if  ($t1h > $t2h + 2) 
            {
                // left is heavier than right
                if ($t1 instanceof MapNode)
                {
                    $t1k = $t1->key;
                    $t1v = $t1->value;
                    $t1l = $t1->left;
                    $t1r = $t1->right; 
                    // one of the nodes must have height > height t2 + 1 
                    if (MapTree::height($t1r) > $t2h + 1) 
                    {
                        // balance right: combination 
                        if (!($t1r instanceof MapNode))
                            throw new Error("rebalance");
                        $t1rk = $t1r->key;
                        $t1rv = $t1r->value;
                        $t1rl = $t1r->left;
                        $t1rr = $t1r->right;
                        return MapTree::mk(MapTree::mk($t1l, $t1k, $t1v, $t1rl), $t1rk, $t1rv, MapTree::mk($t1rr, $k, $v, $t2));
                    }
                    else
                        return MapTree::mk($t1l, $t1k, $t1v, MapTree::mk($t1r, $k, $v, $t2));
                }
                else
                {
                    throw new Error("rebalance");
                }
            }
            else 
            {
                return MapTree::mk($t1, $k, $v, $t2);
            }
        }
    }

    static function add($comparer, $k, $v, $m)
    { 
        if (is_null($m))
            return new MapOne($k, $v);
        if ($m instanceof MapNode)
        {
            $k2 = $m->key;
            $v2 = $m->value;
            $l = $m->left;
            $r = $m->right;
            $h = $m->height;
            $c = $comparer['Compare']($k, $k2); 
            if ($c < 0) 
                return MapTree::rebalance(MapTree::add($comparer, $k, $v, $l), $k2, $v2, $r);
            elseif ($c == 0) 
                return new MapNode($k, $v, $l, $r, $h);
            else
                return MapTree::rebalance($l, $k2, $v2, MapTree::add($comparer, $k, $v, $r)); 
        }
        $k2 = $m->key; 
        $c = $comparer['Compare']($k, $k2); 
        if ($c < 0) 
            return new MapNode($k, $v, NULL, $m, 2);
        elseif ($c == 0)
            return new MapOne($k, $v);
        else
            return new MapNode($k, $v, $m, NULL, 2);
    }


    static function tryGetValue($comparer, $k, &$v, $m)
    { 
        if (is_null($m))
            return false;
        if ($m instanceof MapNode)
        {
            $k2= $m->key;
            $v2= $m->value;
            $l= $m->left;
            $r= $m->right;
            $c = $comparer['Compare']($k, $k2); 
            if ($c < 0) 
                return MapTree::tryGetValue($comparer, $k, $v, $l);
            elseif ($c == 0) 
                { $v = $v2; 
                    return true; }
            else 
                return MapTree::tryGetValue($comparer, $k, $v, $r);
        }
        $k2 = $m->key;
        $v2 = $m->value;
        $c = $comparer['Compare']($k, $k2); 
        if ($c == 0)
            { $v = $v2;
                return true;
            }
        else
            return false;
    }

    static function find($comparer, $k, $m)
    {
        if (MapTree::tryGetValue($comparer, $k, $v, $m))
            return $v;
        else
            throw new Exception("Key not found");
    }

    static function tryFind($comparer, $k, $m)
    { 
        if (MapTree::tryGetValue($comparer, $k, $v, $m))
            return $v;
        else
            return NULL;
    }

    static function fold($f, $x, $m)
    {
        if (is_null($m))
            return $x;

        if ($m instanceof MapNode)
        {
            $x = MapTree::fold($f, $x, $m->left);
            $x = $f($x, $m->key, $m->value);
            return MapTree::fold($f, $x, $m->right);
        }
        return $f($x, $m->key, $m->value);
    }

    static function exists($f, $m)
    { 
        if (is_null($m))
            return false;
        if ($m instanceof MapNode)
        {
            return 
                MapTree::exists($f, $m->left)
                || $f($m->key, $m->value)
                || MapTree::exists($f, $m->right);

        }
        return $f($m->key, $m->value);
    }

    static function mapi($f,$m)
    {
        if (is_null($m))
            return $m;
        if ($m instanceof MapNode)
        {
            $k = $m->key;
            $l2 = MapTree::mapi($f, $m->left); 
            $v2 = $f($k, $m->value); 
            $r2 = MapTree::mapi($f, $m->right); 
            return new MapNode ($k, $v2, $l2, $r2, $m->height);
        }
        return new MapOne ($m->key, $f($m->key, $m->value));
    }
}

class Map implements IteratorAggregate
{
    // This type is logically immutable. This field is only mutated during deserialization.
    public $Comparer;
 
    // This type is logically immutable. This field is only mutated during deserialization.
    public $Tree;

    function __construct($comparer, $tree)
    {
        if (is_null($comparer))
        {
            throw new Exception("Comparer is NULL");
        }
        $this->Comparer = $comparer;
        $this->Tree = $tree;
    }

    function get_Item($key)
    {
        return MapTree::find($this->Comparer, $key, $this->Tree);
    }

    public function getIterator() {
        $stack = [];
        $tree = $this->Tree;
        while(!(is_null($tree) && empty($stack)))
        {
            if (is_null($tree))
                $tree = array_pop($stack);
            elseif ($tree instanceof MapNode)
            {
                array_push($stack, $tree->right);
                array_push($stack, new MapOne($tree->key, $tree->value));
                $tree = $tree->left;
            }
            else {
                    yield [$tree->key, $tree->value];
                    $tree = array_pop($stack);
            }
        }
    }
} 

function _empty()  
{
    $comparer = [ 'Compare' => '\\Util\\compare' ];

    return new Map($comparer, NULL);
} 

function count($table)
{
    return MapTree::size($table->Tree);
}

function ofList($list)
{
    $tree = NULL;
    $comparer = [ 'Compare' => '\\Util\\compare' ];

    while ($list instanceof Cons)
    {
        $tree = MapTree::add($comparer, $list->value[0], $list->value[1], $tree);
        $list = $list->next;
    }


    return new Map($comparer, $tree);
}

function ofSeq($seq)
{

    $tree = NULL;
    $comparer = [ 'Compare' => '\\Util\\compare' ];

    foreach ($seq as $item)
    {
        $tree = MapTree::add($comparer, $item[0], $item[1], $tree);
    }

    return new Map($comparer, $tree);
}

function ofArray($seq)
{
    $tree = NULL;
    $comparer = [ 'Compare' => '\\Util\\compare' ];

    foreach ($seq as $item)
    {
        $tree = MapTree::add($comparer, $item[0], $item[1], $tree);
    }

    return new Map($comparer, $tree);
}

function add($key, $value, $table)
{
    return new Map($table->Comparer, MapTree::add($table->Comparer, $key, $value, $table->Tree));
}

function find($key, $table)
{

    return MapTree::find($table->Comparer,$key,$table->Tree);
}
function tryFind($key, $table)
{
    return MapTree::tryFind($table->Comparer,$key,$table->Tree);
}


function toSeq($table)
{
    return $table;
}

function toList($table)
{
    return \FSharpList\ofSeq($table);
}

function fold($f,$acc,$table)
{
    return MapTree::fold($f,$acc,$table->Tree);
}

function exists($f, $table)
{
    return MapTree::exists($f, $table->Tree);
}
function map($f, $table)
{
    return new Map($table->Comparer, MapTree::mapi($f, $table->Tree));
}
