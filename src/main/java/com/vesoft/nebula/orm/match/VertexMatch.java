/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.ngql.Column;
import com.vesoft.nebula.orm.operator.AggregateFunction;
import com.vesoft.nebula.orm.operator.Filter;
import com.vesoft.nebula.orm.operator.Sort;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * match Vertex
 */
public class VertexMatch {
    protected Graph graph;
    private String tagName;
    private long skip = 0;
    private long limit;
    private HashMap<Column, Sort> orderBy;
    private List<String> filterString;
    private HashMap<String, Filter> conMap;
    private HashMap<String, Object> propMap;
    private List<Column> groupBy;
    private List<AggregateFunction> aggregateFunctions;

    protected VertexMatch(Graph graph) {
        this.graph = graph;
    }

    /**
     * @param tagName tagName
     * @param propMap if you create tag index,you can pass in propMap
     *                eg: match (v:player{name: "qkm"})
     */
    protected void init(String tagName, HashMap<String, Object> propMap) {
        this.tagName = tagName;
        this.propMap = propMap;
    }

    /**
     * /**
     * filter condition
     *
     * <p>For conMap,if you represents a relationship ,you can pass in
     * <"name",Relational.EQ.setValue("qkm")>it means v.name == "qkm".</p>
     * <p>if you represents a logic relationship you can pass in
     * <"name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),Relational.EQ.setValue("SC"))>
     * it means v.name == "qkm" or v.name == "SC";</p>
     * <p>all map elements represents an and logical relationship</p>
     *
     * @param conMap       String is propName,Condition is {@link Filter}
     *                     include (Relational、Logical、UnaryOperate)
     * @param filterString filterString is alternative ,you can pass in
     *                     "v.name == "qkm"",it same to pass in
     *                     <"name",Relational.EQ.setValue("qkm")> for conMap,
     *                     TODO check format.
     * @return VertexMatch
     */
    public VertexMatch where(HashMap<String, Filter> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return this;
    }

    /**
     * @param skip return from line skip of the result
     * @return VertexMatch
     */
    public VertexMatch skip(long skip) {
        this.skip = skip;
        return this;
    }

    /**
     * @param orderBy sort by one or multiple attribute,pass in eg: (v.name,Sort.ASC)
     * @return VertexMatch
     */
    public VertexMatch orderBy(HashMap<Column, Sort> orderBy) {
        this.orderBy = orderBy;
        return this;
    }

    /**
     * achieve group by use aggregateFunctions.eg:return v.name,max(v.age),
     * at the same time, you can optionally pass in aliases.
     *
     * @param groupBy            for grouping,if you only want to implement aggregation,
     *                           the alias in the column is optional,
     *                           you can only pass the {@link Column}(name, null).
     *                           If you use group by and order by, you must pass in alias.
     * @param aggregateFunctions for calculation
     * @return VertexMatch
     */
    public VertexMatch groupBy(List<Column> groupBy, AggregateFunction... aggregateFunctions) {
        this.groupBy = groupBy;
        this.aggregateFunctions = Arrays.asList(aggregateFunctions);
        return this;
    }

    /**
     * @param limit start from line 0 of the default value and end with line limit,
     *              it can be used in combination with skip,skip can be change default start value
     * @return VertexMatch
     */
    public VertexMatch limit(long limit) {
        this.limit = limit;
        return this;
    }

    /**
     * @return from result get the first Vertex
     */
    public ResultSet.Record first() {
        if (all().rowsSize() != 0) {
            return all().rowValues(0);
        }
        return null;
    }

    /**
     * connect parameters
     * match (v:%s)
     *
     * @return sentence
     */
    private String connectQueryParameters() {
        StringBuilder result = new StringBuilder();
        result.append(String.format("MATCH (v%s) ", Match.joinTag(tagName, propMap)));
        result.append(Match.judgeAndJoinWhere(conMap, filterString, 0));
        result.append(Match.joinGroupByAndOrderBy(groupBy, aggregateFunctions, orderBy));
        result.append(Match.joinSkipAndLimit(skip, limit));
        return result.toString();
    }

    /**
     * @return ResultSet
     */
    public ResultSet all() {
        String matchVertex = connectQueryParameters();
        ResultSet resultSet = graph.run(matchVertex);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        return resultSet;
    }

    /**
     * count of results
     *
     * @return count
     */
    public long count() {
        return all().rowsSize();
    }

    /**
     * is there data that meets the conditions
     *
     * @return true or false
     */
    public boolean exist() {
        return count() > 0;
    }
}
