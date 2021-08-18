/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.ngql;

import com.vesoft.nebula.Date;
import com.vesoft.nebula.orm.datatype.DateTime;
import com.vesoft.nebula.orm.datatype.Time;
import com.vesoft.nebula.orm.entity.Property;
import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.entity.Schema;
import com.vesoft.nebula.orm.entity.Vertex;
import com.vesoft.nebula.orm.exception.DataTypeException;
import com.vesoft.nebula.orm.exception.InitException;
import java.util.*;

/**
 * used to splice objects into nGql statements.
 *
 * @author Qi Kai Meng
 */
public class Encoding {
    /**
     * splice the tags in the map into tagString.
     *
     * @param propMap tagMap
     * @return tagString spliced by tagMap, eg:player(name,age)
     */
    public static String joinTag(HashMap<String,
        HashMap<String, Object>> propMap) {
        ArrayList<String> tagStringList = new ArrayList<>();
        StringBuilder tagPartFormat = new StringBuilder();
        StringBuilder tagPart = new StringBuilder();
        for (String tagName : propMap.keySet()) {
            tagPart.append("`").append(tagName).append("`").append("(%s)");
            HashMap<String, Object> propValueMap = propMap.get(tagName);
            if (propValueMap != null) {
                if (propValueMap.containsKey(null)) {
                    throw new DataTypeException("attribute name cannot be null");
                }
                tagPartFormat.append(String.format(tagPart.toString(),
                    useSymbolSplitAddBackQuote(new ArrayList<>(propValueMap.keySet()), ",")));
            } else {
                tagPartFormat.append(String.format(tagPart.toString(), ""));
            }
            tagPart.delete(0, tagPart.length());
            tagStringList.add(tagPartFormat.toString());
            tagPartFormat.delete(0, tagPartFormat.length());
        }
        return String.join(",", tagStringList);
    }

    /**
     * use parameter to connect a attribute values of vertex.
     *
     * @param vertex vertex object
     * @return attribute values of vertex, eg:"1":("qkm",19)
     * @throws DataTypeException the data type passed in does not match the given condition
     */
    public static String joinVertexValue(Vertex vertex) {
        ArrayList<String> values = new ArrayList<>();
        String result = "%s:(%s)";
        HashMap<String, HashMap<String, Object>> propMap = vertex.getPropMap();
        for (String tagName : propMap.keySet()) {
            HashMap<String, Object> valueMap = propMap.get(tagName);
            if (valueMap == null || valueMap.isEmpty()) {
                continue;
            }
            for (String propName : valueMap.keySet()) {
                Object object = valueMap.get(propName);
                values.add(judgeDataType(object));
            }
        }
        return String.format(result, (vertex.getVid() instanceof String)
            ? "\"" + vertex.getVid() + "\"" : vertex.getVid(), String.join(",", values));
    }

    /**
     * use edgeName and attribute name to connect.
     *
     * @param edgeName     edgeName
     * @param propNameList propNameList
     * @return edgeTypeString, eg: relation(name,salve).
     * @throws DataTypeException the data type passed in does not match the given condition
     */
    public static String joinEdge(String edgeName, Set<String> propNameList) {
        if (edgeName == null) {
            throw new DataTypeException("edgeType name cannot be null");
        }
        if (propNameList == null || propNameList.size() == 0) {
            return String.format("`%s`(%s)", edgeName, "");
        }
        return String.format("`%s`(%s)", edgeName, String.join(",", propNameList));

    }

    /**
     * use parameter to connect a attribute values of relationship.
     *
     * @param relationship a relationship object
     * @return edgeValues eg:"1"->"2":("qkm",19)
     * @throws DataTypeException the data type passed in does not match the given condition
     */
    public static String joinRelationshipValue(Relationship relationship) {
        if (relationship.getStartVid() == null || relationship.getEndVid() == null) {
            throw new InitException("vid cannot be null");
        }
        ArrayList<String> values = new ArrayList<>();
        if (relationship.getPropMap() != null && !relationship.getPropMap().isEmpty()) {
            for (String value : relationship.getPropMap().keySet()) {
                Object object = relationship.getPropMap().get(value);
                values.add(judgeDataType(object));
            }
        }
        return String.format("%s->%s@%d:(%s)",
            relationship.getStartVid() instanceof String
                ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
            relationship.getEndVid() instanceof String
                ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
            relationship.getRank(), String.join(",", values));
    }

    public static String encodeDate(Date date) {
        return String.format("%d-%02d-%02d", date.getYear(), date.getMonth(),
            date.getDay());
    }

    public static String encodeList(List<?> list) {
        ArrayList<String> listString = new ArrayList<>();
        for (Object o : list) {
            listString.add(judgeDataType(o));
        }
        return listString.toString();
    }

    public static String encodeIdList(List<?> vidList) {
        ArrayList<String> revertVid = new ArrayList<>();
        for (Object id : vidList) {
            if (id instanceof String) {
                revertVid.add("\"" + id + "\"");
            } else {
                revertVid.add(id.toString());
            }
        }
        return String.join(",", revertVid);
    }

    /**
     * judge object what data type is it.
     *
     * @param object object
     * @return data format
     * @throws DataTypeException the data type passed in does not match the given condition
     */
    public static String judgeDataType(Object object) {
        if (object == null) {
            return "null";
        } else if (object instanceof Integer || object instanceof Float
            || object instanceof Double || object instanceof Byte
            || object instanceof Long || object instanceof Short) {
            return object.toString();
        } else if (object instanceof String) {
            return "\"" + object + "\"";
        } else if (object instanceof DateTime) {
            return String.format("datetime(\"%s\")", ((DateTime) object).getDateTimeString());
        } else if (object instanceof Time) {
            return String.format("time(\"%s\")", ((Time) object).getTimeString());
        } else if (object instanceof Date) {
            return String.format("date(\"%s\")", encodeDate((Date) object));
        } else if (object instanceof Boolean) {
            return String.format("%s", object);
        } else if (object instanceof List) { // has question
            return encodeList((List<?>) object);
        } else {
            throw new DataTypeException(String.format("nGql does not support type %s",
                object.getClass().getName()));
        }
    }

    /**
     * add BackQuote for attribute name and split use comma
     *
     * @param nameList name List
     * @return (% s, % s, % s)
     */
    public static String useSymbolSplitAddBackQuote(List<String> nameList, String symbol) {
        ArrayList<String> propNameAddBackQuote = new ArrayList<>();
        for (String name : nameList) {
            propNameAddBackQuote.add("`" + name + "`");
        }
        return String.join(symbol, propNameAddBackQuote);
    }

    /**
     * sentence of create schema
     *
     * @param schema tag or edge
     * @return create sentence
     */
    public static String joinSchema(Schema schema) {
        List<Property> propertyList = schema.getPropertyList();
        StringBuilder prop = new StringBuilder();
        ArrayList<String> expired = new ArrayList<>();
        if (propertyList != null && !propertyList.isEmpty()) {
            prop.append(traversalProp(propertyList));
            if (schema.getTtlDuration() != 0) {
                expired.add("TTL_DURATION = " + schema.getTtlDuration());
            }
            if (schema.getTtlCol() != null) {
                expired.add("TTL_COL = \"" + schema.getTtlCol() + "\"");
            }
        }
        return String.format("`%s`(%s)%s",
            schema.getName(), prop, String.join(",", expired));
    }

    /**
     * traversal property List
     *
     * @param propertyList property List
     * @return `name` string not null default 20,`age` int not null
     */
    public static String traversalProp(List<Property> propertyList) {
        ArrayList<String> prop = new ArrayList<>();
        StringBuilder part = new StringBuilder();
        for (Property property : propertyList) {
            part.append("`").append(property.getPropName()).append("` ")
                .append(property.getDataType().toString().equals("FIXED_STRING")
                    ? String.format("FIXED_STRING(%d)",
                    property.getDataType().getLength()) : property.getDataType()).append(" ");
            if (property.isNullable()) {
                part.append("NULL");
            } else {
                part.append("NOT NULL");
            }
            if (property.getDefaultValue() != null) {
                part.append(" DEFAULT ").append(Encoding.judgeDataType(property.getDefaultValue()));
            }
            prop.add(part.toString());
            part.delete(0, part.length());
        }
        return String.join(",", prop);
    }

    /**
     * @param propMap attribute value of schema
     * @return eg: set name = "qkm",age = 10
     */
    public static String connectProp(HashMap<String, Object> propMap) {
        ArrayList<String> keyValue = new ArrayList<>();
        for (String propName : propMap.keySet()) {
            keyValue.add(String.format("%s: %s", propName, judgeDataType(propMap.get(propName))));
        }
        return String.join(",", keyValue);
    }

    /**
     * connect the property to be indexed with the index length to be set.
     *
     * @param indexMap indexMap,String is attribute name ,Integer is index length
     */
    public static String joinIndexProp(HashMap<String, Integer> indexMap) {
        ArrayList<String> indexList = new ArrayList<>();
        if (indexMap == null) {
            return null;
        }
        for (String propName : indexMap.keySet()) {
            if (indexMap.get(propName) != null) {
                indexList.add(String.format("%s(%d)", propName, indexMap.get(propName)));
            } else {
                indexList.add(String.format("%s", propName));
            }

        }
        return String.join(",", indexList);
    }
}