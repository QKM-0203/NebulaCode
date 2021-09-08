/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.operator.Filter;
import com.vesoft.nebula.orm.operator.Sort;
import com.vesoft.nebula.orm.query.cypher.Lexer;
import com.vesoft.nebula.orm.query.ngql.Column;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * <p>you can use {@link #where(Map, String...)} for conditional filtering,
 * and use {@link #skip(long)}, {@link #limit(long)}, {@link #groupBy(List, List)}}
 * and {@link #orderBy(Map)} to operate the output results.</p>
 * <p>the user does not need to consider the calling order,
 * when the user calls the {@link #all()}、{@link #first()} etc method,
 * the parameter connection will be made.</p>
 * <p>note: make sure that at least one index is available for the match statement.</p>
 *
 * @author Qi Kai Meng
 */
public class VertexMatch {
    protected Graph graph;
    private String tagName;
    private long skip = 0;
    private long limit = -1;
    private Map<Column, Sort> orderBy;
    private List<String> filterString;
    private Map<String, Filter> conMap;
    private Map<String, Object> propMap;
    private List<Column> groupBy;
    private List<Column> aggregateFunctions;

    protected VertexMatch(Graph graph) {
        this.graph = graph;
    }

    /**
     * if you don't set the index, you can find it by id (v) at after where.
     *
     * @param tagName if vertex has tag index,you can pass in,eg:
     *                match (v:player),can be null eg: match (v).
     * @param propMap if you create tag prop index,you can pass in propMap
     *                eg: match (v:player{name: "qkm"}),can be null eg:
     *                match (v:player).
     */
    protected VertexMatch init(String tagName, Map<String, Object> propMap) {
        this.tagName = tagName;
        this.propMap = propMap;
        return this;
    }

    /**
     * filter condition,finally, conMap and filterString do logical sum operations.
     *
     * <p>for conMap,if you represents a Vertex ,you can pass in
     * <"name",Relational.EQ.setValue("qkm")>it means v.name == "qkm".</p>
     * <p>if you represents a logic relationship you can pass in
     * <"name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),Relational.EQ.setValue("SC"))>
     * it means v.name == "qkm" or v.name == "SC".</p>
     * <p>all map elements represents an and logical relationship.</p>
     *
     * @param conMap       String is propName,Condition is {@link Filter}
     *                     include (Relational、Logical、UnaryOperate)
     * @param filterString filterString is alternative ,you can pass in
     *                     "v.name == "qkm"",it same to pass in
     *                     <"name",Relational.EQ.setValue("qkm")> for conMap TODO check format
     * @return VertexMatch
     */
    public VertexMatch where(Map<String, Filter> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return this;
    }

    /**
     * if pass in negative means from 0 start
     *
     * @param skip return from line skip of the result
     * @return VertexMatch
     */
    public VertexMatch skip(long skip) {
        this.skip = skip;
        return this;
    }

    /**
     * <p>if order by and group by are used together,the field of orderBy passed must be
     * included in {@link #groupBy(List, List)}groupBy and aggregateFunctions,
     * this function will sort directly using the passed alias,you can pass in
     * <{@link Column}(null,alias),{@link Sort}>.</p>
     * <p>if you only use order by,this function puts the orderBy field you passed in
     * after the return field as output,and then sorts the aliases,you must pass in
     * <{@link Column}("propName",alias),{@link Sort}>.</p>
     * <p>if you do not use order by, return v.</p>
     * <p>order by uses an alias to sort,so you must pass in an alias,
     * if the sort you passed is null,default is ASC.</p>
     *
     * @param orderBy sort by one or multiple attribute
     * @return VertexMatch
     */
    public VertexMatch orderBy(Map<Column, Sort> orderBy) {
        this.orderBy = orderBy;
        return this;
    }

    /**
     * achieve group by use aggregateFunctions.eg:return v.name [as name],max(v.age) [as max].
     *
     * <p>if group by and order by are used together,for the field you need order by,
     * you must pass in alias.eg:{@link Column}(propName, alias) or
     * {@link Column}(AggregateFunction,alias).</p>
     * <p>if you only use group by,the alias in the column is optional,
     * you can only pass the {@link Column}(propName, null) or
     * {@link Column}(AggregateFunction,null).</p>
     *
     * @param groupBy            for grouping
     * @param aggregateFunctions for calculation
     * @return VertexMatch
     */
    public VertexMatch groupBy(List<Column> groupBy, List<Column> aggregateFunctions) {
        this.groupBy = groupBy;
        this.aggregateFunctions = aggregateFunctions;
        return this;
    }

    /**
     * if you pass in negative means show all results.
     *
     * @param limit start from line 0 of the default value and end with line limit,
     *              it can be used in combination with skip,skip can be change default start value.
     * @return VertexMatch
     */
    public VertexMatch limit(long limit) {
        this.limit = limit;
        return this;
    }

    /**
     * connect parameters.
     *
     * @return sentence
     */
    private String connectParameters() {
        StringBuilder result = new StringBuilder();
        result.append(String.format(Lexer.MATCH + "(v%s)", Match.joinTag(tagName, propMap)));
        result.append(Match.judgeAndJoinWhere(conMap, filterString, 0));
        result.append(Match.joinGroupByAndOrderBy(groupBy, aggregateFunctions, orderBy, 0));
        result.append(Match.joinSkipAndLimit(skip, limit));
        return result.toString().trim();
    }

    /**
     * @return from result get the first
     */
    public ResultSet.Record first() {
        ResultSet all = all();
        if (!all.isEmpty()) {
            return all.rowValues(0);
        }
        return null;
    }

    /**
     * @return ResultSet
     */
    public ResultSet all() {
        String matchVertex = connectParameters();
        ResultSet resultSet = graph.run(matchVertex);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        return resultSet;
    }

    /**
     * count of results.
     *
     * @return count
     */
    public long count() {
        ResultSet all = all();
        if (!all.isEmpty()) {
            return all.rowsSize();
        }
        return 0;
    }

    /**
     * is there data that meets the conditions.
     *
     * @return true or false
     */
    public boolean exist() {
        return !all().isEmpty();
    }
}
