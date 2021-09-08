/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.operator.Sort;
import com.vesoft.nebula.orm.query.cypher.Lexer;
import com.vesoft.nebula.orm.query.cypher.QueryBase;
import com.vesoft.nebula.orm.query.util.Util;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * fields used to join NGql queries.
 *
 * @author Qi Kai Meng
 */
public class NGqlQuery extends QueryBase {
    public static String joinRelationshipNoHasValue(List<Relationship> relationships) {
        ArrayList<String> result = new ArrayList<>();
        for (Relationship relationship : relationships) {
            result.add(String.format(" %s->%s@%s", relationship.getStartVid() instanceof String
                    ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                relationship.getEndVid() instanceof String
                    ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                relationship.getRank()));
        }
        return String.join(",", result);
    }

    public static String joinOrderBy(Map<String, Sort> orderBy) {
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

    public static String joinGroupBy(List<Column> groupBy, List<Column> aggregateFunctions) {
        StringBuilder result = new StringBuilder();
        if (groupBy != null && !groupBy.isEmpty()) {
            result.append(String.format(Lexer.PIPE + Lexer.GROUP_BY + "%s", joinAttribute(groupBy)));
            result.append(String.format(Lexer.YIELD + " %s,%s ", joinAttributeAlias(groupBy),
                joinAggregateFunctionsAlias(aggregateFunctions)));
        }
        return result.toString();
    }

    public static List<String> joinFetch(List<Relationship> relationships, List<String> yield) {
        HashMap<String, List<Relationship>> joinSameEdgeRelationships =
            Util.joinSameEdgeRelationships(relationships, 1);
        ArrayList<String> fetchStrings = new ArrayList<>();
        for (String edgeName : joinSameEdgeRelationships.keySet()) {
            StringBuilder fetch = new StringBuilder();
            fetch.append(Lexer.FETCH_PROP_ON).append(edgeName);
            fetch.append(joinRelationshipNoHasValue(joinSameEdgeRelationships.get(edgeName)));
            if (yield != null && !yield.isEmpty()) {
                fetch.append(Lexer.YIELD).append(String.join(",", yield));
            }
            fetchStrings.add(fetch.toString());
        }
        return fetchStrings;
    }
}
