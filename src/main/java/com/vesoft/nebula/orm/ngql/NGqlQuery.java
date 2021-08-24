/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.ngql;

import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.operator.Sort;
import com.vesoft.nebula.orm.util.QueryBase;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * fields used to join NGql queries.
 */
public class NGqlQuery extends QueryBase {
    public static String joinRelationshipNoHasValue(List<Relationship> relationships) {
        ArrayList<String> result = new ArrayList<>();
        for (Relationship relationship : relationships) {
            result.add(String.format("%s->%s@%s", relationship.getStartVid() instanceof String
                    ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                relationship.getEndVid() instanceof String
                    ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                relationship.getRank()));
        }
        return String.join(",", result);
    }

    public static String joinOrderBy(HashMap<String, Sort> orderBy) {
        ArrayList<String> orderByStrings = new ArrayList<>();
        for (String propName : orderBy.keySet()) {
            if (orderBy.get(propName) != null) {
                orderByStrings.add(propName + " " + orderBy.get(propName));
            } else {
                orderByStrings.add(propName);
            }
        }
        return String.join(",", orderByStrings);
    }

    public static String joinGroupBy(List<AttributeColumn> groupBy, List<FunctionColumn> aggregateFunctions) {
        StringBuilder result = new StringBuilder();
        if (groupBy != null && !groupBy.isEmpty()) {
            result.append(String.format("| GROUP BY %s ", joinAttribute(groupBy)));
            result.append(String.format("YIELD %s,%s", joinAttributeAlias(groupBy),
                joinAggregateFunctionsAlias(aggregateFunctions)));
        }
        return result.toString();
    }
}
