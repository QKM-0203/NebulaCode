/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.operator.Filter;
import com.vesoft.nebula.orm.operator.Sort;
import com.vesoft.nebula.orm.query.QueryBase;
import com.vesoft.nebula.orm.query.cypher.Encoding;
import com.vesoft.nebula.orm.query.ngql.Column;
import com.vesoft.nebula.orm.query.util.KeyWord;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * fields used to join match queries.
 *
 * @author Qi Kai Meng
 */
public class Match<E> extends QueryBase<E> {
    protected long skip = 0;
    protected long limit = -1;
    protected List<String> filterString;
    protected Map<String, Filter> conMap;
    protected List<Column> groupBy;
    protected List<Column> aggregateFunctions;


    protected Match(Graph graph) {
        super(graph);
    }

    public String joinTag(String tagName, Map<String, Object> tagMap) {
        if (tagName == null) {
            return "";
        } else if (tagMap == null || tagMap.isEmpty()) {
            return String.format(":`%s`", tagName);
        } else {
            return String.format(":`%s`{%s}", tagName, Encoding.connectProp(tagMap));
        }
    }

    public String joinEdge(Map<String, Object> edgeMap, List<String> types) {
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

    private String joinOrderByAlias(Map<Column, Sort> orderBy) {
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

    public String joinGroupByAndOrderBy(List<Column> groupBy,
                                        List<Column> aggregateFunctions,
                                        Map<Column, Sort> orderBy, int isEdgeOrNode) {
        StringBuilder result = new StringBuilder();
        if (groupBy != null && !groupBy.isEmpty()) {
            result.append(" ").append(String.format(KeyWord.RETURN + " %s,%s",
                joinAttributeAlias(groupBy), joinAggregateFunctionsAlias(aggregateFunctions)));
            if (orderBy != null && !orderBy.isEmpty()) {
                result.append(" ").append(KeyWord.ORDER_BY)
                    .append(" ").append(joinOrderByAlias(orderBy));
            }
        } else {
            if (orderBy == null || orderBy.isEmpty()) {
                if (isEdgeOrNode == 0) {
                    result.append(" ").append(KeyWord.RETURN).append(" v ");
                } else {
                    result.append(" ").append(KeyWord.RETURN).append(" e ");
                }
            } else {
                ArrayList<Column> columns = new ArrayList<>(orderBy.keySet());
                result.append(" ").append(KeyWord.RETURN).append(" ")
                    .append(joinAttributeAlias(columns));
                result.append(" ").append(KeyWord.ORDER_BY).append(" ")
                    .append(joinOrderByAlias(orderBy));
            }
        }
        return result.toString();
    }

    public String joinSkipAndLimit(long skip, long limit) {
        StringBuilder result = new StringBuilder();
        if (skip >= 1) {
            result.append(" ").append(KeyWord.SKIP).append(" ").append(skip);
        }
        if (limit >= 0) {
            result.append(" ").append(KeyWord.LIMIT).append(" ").append(limit);
        }
        return result.toString();
    }

    /**
     * achieve group by use aggregateFunctions.eg:return v.name [as name],max(v.age) [as max].
     *
     * <p>if group by and order by are used together,for the field you need order by,
     * you must pass in alias.eg:{@link Column}(propName,alias) or
     * {@link Column}(AggregateFunction,alias).</p>
     * <p>if you only use group by,the alias in the column is optional,
     * you can only pass the {@link Column}(propName, null) or
     * {@link Column}(AggregateFunction,null).</p>
     *
     * @param groupBy            for grouping
     * @param aggregateFunctions for calculation
     * @return RelationshipMatch or VertexMatch
     */
    public E groupBy(List<Column> groupBy, List<Column> aggregateFunctions) {
        this.groupBy = groupBy;
        this.aggregateFunctions = aggregateFunctions;
        return (E) this;
    }

    /**
     * if pass in negative means from 0 start.
     *
     * @param skip return from line skip of the result
     * @return RelationshipMatch or VertexMatch
     */
    public E skip(long skip) {
        this.skip = skip;
        return (E) this;
    }

    /**
     * if you pass in negative means show all results.
     *
     * @param limit start from line 0 of the default value and end with line limit,
     *              it can be used in combination with skip,skip can be change default start value.
     * @return RelationshipMatch or VertexMatch
     */
    public E limit(long limit) {
        this.limit = limit;
        return (E) this;
    }

    /**
     * filter condition,finally, conMap and filterString do logical sum operations.
     *
     * <p>for conMap,if you represents a relationship,you can pass in
     * <"name",Relational.EQ.setValue("qkm")>it means e.name == "qkm".</p>
     * <p>for conMap,if you represents a Vertex ,you can pass in
     * <"name",Relational.EQ.setValue("qkm")>it means v.name == "qkm".</p>
     * <p>if you represents a logic relation you can pass in
     * <"name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),Relational.EQ.setValue("SC"))>
     * for relationship it means e.name == "qkm" or e.name == "SC".
     * for vertex it means v.name == "qkm" or v.name == "SC".</p>
     * <p>all map elements represents an and logical relationship.</p>
     *
     * @param conMap       String is propName,Relational is {@link Filter}
     *                     include (Relational、Logical、UnaryOperate)
     * @param filterString filterString is alternative ,you can pass in
     *                     "e.name == "qkm"" or "v.name == "qkm"" or "v1.name == "qkm""
     *                     or front same to pass in <"name",Relational.EQ.setValue("qkm")>
     *                     for conMap TODO check format
     * @return RelationshipMatch or VertexMatch
     */
    public E where(Map<String, Filter> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return (E) this;
    }
}
