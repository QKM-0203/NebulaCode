/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.operator.Sort;
import com.vesoft.nebula.orm.query.QueryBase;
import com.vesoft.nebula.orm.query.util.KeyWord;
import com.vesoft.nebula.orm.query.util.Util;
import java.util.*;

/**
 * fields used to join NGql queries.
 *
 * @author Qi Kai Meng
 */
public class NGqlQuery<E> extends QueryBase {
    public List<String> yields;

    private String joinRelationshipNoHasValue(List<Relationship> relationships) {
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

    public String joinOrderBy(Map<String, Sort> orderBy) {
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

    public String joinGroupBy(List<Column> groupBy, List<Column> aggregateFunctions) {
        StringBuilder result = new StringBuilder();
        if (groupBy != null && !groupBy.isEmpty()) {
            result.append(String.format(" " + KeyWord.PIPE + " "
                + KeyWord.GROUP_BY + " " + "%s", joinAttribute(groupBy)));
            result.append(String.format(" " + KeyWord.YIELD
                    + " %s,%s ", joinAttributeAlias(groupBy),
                joinAggregateFunctionsAlias(aggregateFunctions)));
        }
        return result.toString();
    }

    public List<String> joinFetch(List<Relationship> relationships) {
        HashMap<String, List<Relationship>> joinSameEdgeRelationships =
            Util.joinSameEdgeRelationships(relationships, 1);
        ArrayList<String> fetchStrings = new ArrayList<>();
        for (String edgeName : joinSameEdgeRelationships.keySet()) {
            StringBuilder fetch = new StringBuilder();
            fetch.append(KeyWord.FETCH_PROP_ON).append(" ").append(edgeName);
            fetch.append(joinRelationshipNoHasValue(joinSameEdgeRelationships.get(edgeName)));
            if (yields != null && !yields.isEmpty()) {
                fetch.append(" ").append(KeyWord.YIELD).append(" ")
                    .append(String.join(",", yields));
            }
            fetchStrings.add(fetch.toString());
        }
        return fetchStrings;
    }

    /**
     * what the user wants to output.
     *
     * @param yield pass in eg:player.name, if you alias output,pass in eg:player.name as name.
     */
    public E yield(String... yield) {
        this.yields = Arrays.asList(yield);
        return (E) this;
    }

    /**
     * what the user wants to output.
     *
     * @param yield pass in eg:player.name, if you alias output,pass in eg:player.name as name,
     *              finally, give the first yield add DISTINCT.
     */
    public E yieldWithDistinct(String... yield) {
        yield[0] = "DISTINCT " + yield[0];
        this.yields = Arrays.asList(yield);
        return (E) this;
    }

}
