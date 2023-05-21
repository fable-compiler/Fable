<?php

namespace Serialization;

require_once "List.php";


// converts an object tree to another that
// can be converted to json
function convertToSimpleJson($obj)
{
    if (is_null($obj)) {
        return null;
    } elseif ($obj instanceof \FSharpList\FSharpList || $obj instanceof \Set\Set) {
        $array = [];
        foreach ($obj as $value) {
            $array[] = convertToSimpleJson($value);
        }

        return $array;
    } elseif ($obj instanceof \FSharpUnion) {
        $vars = get_object_vars($obj);
        $case = $obj->get_FSharpCase();
        if (empty($vars)) {
            return $case;
        }


        if (count($vars) == 1) {
            return array($case => convertToSimpleJson($vars[key($vars)]));
        }

        $props = [];
        foreach ($vars as $prop => $value) {
            $props[] = convertToSimpleJson($value);
        }
        return array($case => $props);
    } elseif (is_array($obj)) {
        $array = [];
        foreach ($obj as $key => $value) {
            $array[$key] = convertToSimpleJson($value);
        }
        return $array;
    } elseif (is_object($obj)) {
        $props = [];
        foreach (get_object_vars($obj) as $prop => $value) {
            $props[$prop] = convertToSimpleJson($value);
        }
        return $props;
    } else {
        return $obj;
    }
}

function convertTypeFromJson($json, $cls)
{
    $class_vars = get_class_vars($cls);
    if (count($class_vars) == 0) {
        return new $cls();
    } elseif (count($class_vars) == 1) {
        $fieldName = key($class_vars);
        $method = "get_{$fieldName}_Type";
        $field = convertFromSimpleJson($json, $cls::$method());
        return new $cls($field);
    } else {
        $fields = [];
        foreach ($class_vars as $field => $_) {
            $method = "get_{$field}_Type";
            $fields[] = convertFromSimpleJson($json->$field, $cls::$method());
        }
        return new $cls(...$fields);
    }
}

function convertFromSimpleJson($json, $cls)
{
    if (is_null($json)) {
        return null;
    }

    if ($cls == 'String') {
        return $json;
    }
    if ($cls == 'Int32') {
        return $json;
    }

    if (is_array($cls)) {
        if ($cls[0] == "List") {
            $result = [];
            foreach ($json as $item) {
                $result[] = convertFromSimpleJson($item, $cls[1]);
            }
            return \FSharpList\ofArray($result);
        } elseif ($cls[0] == "Set") {
            $result = [];
            $itemCls = $cls[1];
            foreach ($json as $item) {
                $result[] = convertFromSimpleJson($item, $itemCls);
            }
            return \Set\ofSeq($result, ['Compare' => '\Util\compare']);
        } elseif ($cls[0] == "Map") {
            $result = [];
            $keyCls = $cls[1];
            $valueCls = $cls[2];
            foreach ($json as $item) {
                $result[] = [
                    convertFromSimpleJson($item[0], $keyCls),
                    convertFromSimpleJson($item[1], $valueCls)
                ];
            }
            return \Map\ofArray($result);
        } elseif ($cls[0] == "Tuple") {
            $result = [];
            $index = 1;
            foreach ($json as $item) {
                $result[] = convertFromSimpleJson($item, $cls[$index]);
                $index++;
            }
            return $result;
        }
    }

    if (is_subclass_of($cls, "FSharpUnion")) {
        if (is_string($json)) {
            foreach ($cls::allCases() as $case) {
                if ($case::get_FSharpCase() == $json) {
                    return new $case();
                }
            }
        } elseif (is_object($json)) {
            foreach ($cls::allCases() as $caseCls) {
                $caseName = $caseCls::get_FSharpCase();
                if (property_exists($json, $caseName)) {
                    return convertTypeFromJson($json->$caseName, $caseCls);
                }
            }
        } elseif (is_array($json)) {
            foreach ($cls::allCases() as $caseCls) {
                $caseName = $caseCls::get_FSharpCase();
                if ($json[$caseName] != null) {
                    return convertTypeFromJson($json[$caseName], $caseCls);
                }
            }
        }
        throw new \Error("Not sure how to decode this union");
    }

    // Record (should be at least iComparable)
    if (is_subclass_of($cls, "\\IComparable")) {
        return convertTypeFromJson($json, $cls);
    }

    throw new \Error("Not sure how to decode this");
}
