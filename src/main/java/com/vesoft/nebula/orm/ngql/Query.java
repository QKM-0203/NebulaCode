/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.ngql;

import com.vesoft.nebula.orm.operator.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * used to splice parameters into query statements
 */
public class Query {

    private static String joinWhere(HashMap<String, Filter> conMap, int flag) {
        ArrayList<String> whereStrings = new ArrayList<>();
        for (String propName : conMap.keySet()) {
            Filter filter = conMap.get(propName);
            if (filter instanceof Logical) {
                whereStrings.add(joinLogical(propName, (Logical) filter,flag));
            } else if (filter instanceof Relational) {
                if (flag == 0) {
                    whereStrings.add(String.format("v.%s %s %s", propName,
                        ((Relational) filter).getSymbol(), Encoding.judgeDataType(((Relational) filter).getValue())));
                } else {
                    whereStrings.add(String.format("e.%s %s %s", propName,
                        ((Relational) filter).getSymbol(), Encoding.judgeDataType(((Relational) filter).getValue())));
                }
            }
        }
        return String.join(" AND ", whereStrings);
    }

    public static String judgeAndJoinWhere(HashMap<String, Filter> conMap, List<String> filterString, int flag) {
        StringBuilder result = new StringBuilder();
        if ((conMap != null && !conMap.isEmpty()) || (filterString != null && !filterString.isEmpty())) {
            result.append("WHERE ");
            if (conMap != null && !conMap.isEmpty()) {
                result.append(joinWhere(conMap, flag));
            }
            if (filterString != null && !filterString.isEmpty()) {
                if (conMap != null && !conMap.isEmpty()) {
                    result.append(" AND ");
                }
                result.append(String.join(" AND ", filterString));
            }
        }
        return result.toString();
    }

    private static String joinLogical(String propName, Logical logical,int flag) {
        Relational leftRelational = logical.getLeftRelational();
        Relational rightRelational = logical.getRightRelational();
        if (flag == 0) {
            return String.format("(%s %s %s)",
                String.format("v.%s %s %s", propName,
                    leftRelational.getSymbol(), Encoding.judgeDataType(leftRelational.getValue())),
                logical.getSymbol(),
                String.format("v.%s %s %s", propName,
                    rightRelational.getSymbol(), Encoding.judgeDataType(rightRelational.getValue())));
        } else {
            return String.format("(%s %s %s)",
                String.format("e.%s %s %s", propName,
                    leftRelational.getSymbol(), Encoding.judgeDataType(leftRelational.getValue())),
                logical.getSymbol(),
                String.format("e.%s %s %s", propName,
                    rightRelational.getSymbol(), Encoding.judgeDataType(rightRelational.getValue())));
        }
    }

    private static String joinOrderByAlias(HashMap<Name, Sort> orderBy) {
        ArrayList<String> orderByStrings = new ArrayList<>();
        for (Name name : orderBy.keySet()) {
            orderByStrings.add(name.getAlias() + " " + orderBy.get(name));
        }
        return String.join(",", orderByStrings);
    }

    public static String joinTag(String tagName, HashMap<String, Object> tagMap) {
        if (tagName == null) {
            return "";
        } else if (tagMap == null || tagMap.isEmpty()) {
            return String.format(":`%s`", tagName);
        } else {
            return String.format(":`%s`{%s}", tagName, Encoding.connectProp(tagMap));
        }
    }

    public static String joinEdge(HashMap<String, Object> edgeMap, List<String> types) {
        if (types != null && !types.isEmpty()) {
            if (types.size() == 1) {
                if (edgeMap == null || edgeMap.isEmpty()) {
                    return String.format("e:`%s`", types.get(0));
                } else {
                    return String.format("e:`%s`{%s}", types.get(0), Encoding.connectProp(edgeMap));
                }
            } else {
                return String.format("e:%s", Encoding.useSymbolSplitAddBackQuote(types,"|:"));
            }
        } else {
            return "e";
        }
    }

    public static String joinGroupByAndOrderBy(List<Name> groupBy,
                                        List<AggregateFunction> aggregateFunctions,
                                        HashMap<Name, Sort> orderBy) {
        StringBuilder result = new StringBuilder();
        if (groupBy != null && !groupBy.isEmpty()) {
            result.append(String.format(" RETURN %s,%s ", joinReturnAlias(groupBy),
                joinAggregateFunctions(aggregateFunctions)));
            if (orderBy != null && !orderBy.isEmpty()) {
                result.append("ORDER BY ").append(joinOrderByAlias(orderBy));
            }
        } else {
            if (orderBy == null || orderBy.isEmpty()) {
                result.append(" RETURN v ");
            } else {
                result.append(" RETURN ").append(joinReturnAlias((List<Name>)orderBy.keySet()));
                result.append(" ORDER BY ").append(joinOrderByAlias(orderBy));
            }
        }
        return result.toString();
    }


    private static String joinAggregateFunctions(List<AggregateFunction> aggregateFunctions) {
        ArrayList<String> result = new ArrayList<>();
        for (AggregateFunction aggregateFunction : aggregateFunctions) {
            result.add(String.format("%s(%s)",aggregateFunction.toString(),aggregateFunction.getValue()));
        }
        return String.join(",",result);
    }

    private static String joinReturnAlias(List<Name> names) {
        ArrayList<String> result = new ArrayList<>();
        for (Name name : names) {
            result.add(name.getPropName() + " AS " + name.getAlias());
        }
        return String.join(",",result);
    }

    public static String joinSkipAndLimit(long skip,long limit) {
        StringBuilder result = new StringBuilder();
        if (skip != 0) {
            result.append(" SKIP ").append(skip);
        }
        if (limit != 0) {
            result.append(" LIMIT ").append(limit);
        }
        return result.toString();
    }

}
