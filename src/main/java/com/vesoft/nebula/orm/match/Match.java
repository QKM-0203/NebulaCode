/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.orm.query.cypher.Encoding;
import com.vesoft.nebula.orm.query.ngql.Column;
import com.vesoft.nebula.orm.operator.Sort;
import com.vesoft.nebula.orm.query.cypher.QueryBase;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * fields used to join match queries.
 */
public class Match extends QueryBase {
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
                return String.format("e:%s", Encoding.useSymbolSplitAddBackQuote(types, "|:"));
            }
        } else {
            return "e";
        }
    }

    private static String joinOrderByAlias(HashMap<Column, Sort> orderBy) {
        ArrayList<String> orderByStrings = new ArrayList<>();
        for (Column column : orderBy.keySet()) {
            if (orderBy.get(column) != null) {
                orderByStrings.add(column.getAlias() + " " + orderBy.get(column));
            } else {
                orderByStrings.add(column.getAlias());
            }
        }
        return String.join(",", orderByStrings);
    }

    public static String joinGroupByAndOrderBy(List<Column> groupBy,
                                               List<Column> aggregateFunctions,
                                               HashMap<Column, Sort> orderBy, int isEdgeOrNode) {
        StringBuilder result = new StringBuilder();
        if (groupBy != null && !groupBy.isEmpty()) {
            result.append(String.format(" RETURN %s,%s ", joinAttributeAlias(groupBy),
                joinAggregateFunctionsAlias(aggregateFunctions)));
            if (orderBy != null && !orderBy.isEmpty()) {
                result.append("ORDER BY ").append(joinOrderByAlias(orderBy));
            }
        } else {
            if (orderBy == null || orderBy.isEmpty()) {
                if (isEdgeOrNode == 0) {
                    result.append(" RETURN v ");
                } else {
                    result.append(" RETURN e ");
                }
            } else {
                result.append(" RETURN ").append(joinAttributeAlias((List<Column>) orderBy.keySet()));
                result.append(" ORDER BY ").append(joinOrderByAlias(orderBy));
            }
        }
        return result.toString();
    }

    public static String joinSkipAndLimit(long skip, long limit) {
        StringBuilder result = new StringBuilder();
        if (skip > 0) {
            result.append(" SKIP ").append(skip);
        }
        if (limit >= 0) {
            result.append(" LIMIT ").append(limit);
        }
        return result.toString();
    }
}
