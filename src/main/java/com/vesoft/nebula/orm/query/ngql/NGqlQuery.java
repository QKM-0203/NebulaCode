/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.query.QueryBase;
import com.vesoft.nebula.orm.query.util.KeyWord;
import com.vesoft.nebula.orm.query.util.Util;
import java.util.*;

/**
 * fields used to join NGql queries.
 *
 * @author Qi Kai Meng
 */
public class NGqlQuery<E> extends QueryBase<E> {
    protected List<String> yields;

    protected NGqlQuery(Graph graph) {
        super(graph);
    }

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

    public List<String> joinFetch(List<Relationship> relationships, StringBuffer condition) {
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
            if (condition != null) {
                fetch.append(condition);
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

    /**
     * get the number of rows of the result.
     *
     * @param offsetValue start from line 0 of the default value and end with line limit.
     * @param numberRows  return some rows.
     * @return Go
     */
    public E limit(long offsetValue, long numberRows) {
        if (this instanceof Go) {
            Go go = (Go) this;
            go.getCondition().append(" ").append(KeyWord.PIPE).append(" ")
                .append(KeyWord.LIMIT).append(" ").append(offsetValue)
                .append(",").append(numberRows).append(" ");
        } else if (this instanceof LookUp) {
            LookUp lookUp = (LookUp) this;
            lookUp.getClause().append(" ").append(KeyWord.PIPE).append(" ")
                .append(KeyWord.LIMIT).append(" ").append(offsetValue)
                .append(",").append(numberRows).append(" ");
        } else if (this instanceof FetchEdge) {
            FetchEdge fetchEdge = (FetchEdge) this;
            fetchEdge.getClause().append(" ").append(KeyWord.PIPE).append(" ")
                .append(KeyWord.LIMIT).append(" ").append(offsetValue)
                .append(",").append(numberRows).append(" ");

        } else if (this instanceof FetchVertex) {
            FetchVertex fetchVertex = (FetchVertex) this;
            fetchVertex.getClause().append(" ").append(KeyWord.PIPE).append(" ")
                .append(KeyWord.LIMIT).append(" ").append(offsetValue)
                .append(",").append(numberRows).append(" ");
        } else if (this instanceof FindPath) {
            FindPath findPath = (FindPath) this;
            findPath.getClause().append(" ").append(KeyWord.PIPE).append(" ")
                .append(KeyWord.LIMIT).append(" ").append(offsetValue)
                .append(",").append(numberRows).append(" ");
        } else if (this instanceof GetSubgraph) {
            GetSubgraph getSubgraph = (GetSubgraph) this;
            getSubgraph.getClause().append(" ").append(KeyWord.PIPE).append(" ")
                .append(KeyWord.LIMIT).append(" ").append(offsetValue)
                .append(",").append(numberRows).append(" ");
        }
        return (E) this;
    }
}
