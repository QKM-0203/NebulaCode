/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.ngql.AttributeColumn;
import com.vesoft.nebula.orm.ngql.FunctionColumn;
import com.vesoft.nebula.orm.operator.EdgeDirection;
import com.vesoft.nebula.orm.operator.Filter;
import com.vesoft.nebula.orm.operator.Sort;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * match Relationship
 * <p>you can use {@link #where(HashMap, String...)} for conditional filtering,
 * and use {@link #skip(long)}, {@link #limit(long)}, {@link #groupBy(List, List)}}
 * and {@link #orderBy(HashMap)} to operate the output results.</p>
 * <p>the user does not need to consider the calling order,
 * when the user calls the {@link #all()}、{@link #first()} etc method,
 * the parameter connection will be made.</p>
 */
public class RelationshipMatch {
    protected Graph graph;
    private String startTagName;
    private String endTagName;
    private List<String> edges;
    private List<String> filterString;
    private long skip = 0;
    private long limit = -1;
    private HashMap<AttributeColumn, Sort> orderBy;
    private List<AttributeColumn> groupBy;
    private EdgeDirection edgeDirection = EdgeDirection.OUT;
    private HashMap<String, Filter> conMap;
    private HashMap<String, Object> startTagMap;
    private HashMap<String, Object> endTagMap;
    private HashMap<String, Object> edgeMap;
    private List<FunctionColumn> aggregateFunctions;

    protected RelationshipMatch(Graph graph) {
        this.graph = graph;
    }

    /**
     * if you don't set the index, you can find it by id (v) or id(v1) at after where.
     *
     * @param startTagName  if startVertex has tag index,you can pass in,eg:
     *                      (v:player),can be null eg: (v).
     * @param startTagMap   if you create tag prop index,you can pass in propMap
     *                      eg: (v:player{name: "qkm"}),can be null eg: (v:player).
     * @param endTagName    if endVertex has tag index,you can pass in,eg:
     *                      (v1:player),can be null eg: (v1).
     * @param endTagMap     if you create tag prop index,you can pass in propMap
     *                      eg: (v1:player{name: "qkm"}),can be null eg: (v1:player).
     * @param edgeDirection in edge or out edge
     * @param edgeMap       if you create edge prop index,you can pass in,eg:
     *                      match (v)-[e:player{name: "qkm"}]-(v1)
     * @param types         edgeName,if you create index on edge you can pass in,
     *                      if you pass in multiple edges,we will only use edge instead of edgeMap.
     *                      eg: [e:player|:team|:work].
     */
    protected void init(String startTagName, HashMap<String, Object> startTagMap,
                        String endTagName, HashMap<String, Object> endTagMap,
                        EdgeDirection edgeDirection, HashMap<String, Object> edgeMap,
                        String... types) {
        this.startTagName = startTagName;
        this.endTagName = endTagName;
        this.startTagMap = startTagMap;
        this.endTagMap = endTagMap;
        this.edgeMap = edgeMap;
        this.edges = Arrays.asList(types);
        this.edgeDirection = edgeDirection;
    }

    /**
     * filter condition.
     *
     * <p>for conMap,if you represents a relationship,you can pass in
     * <"name",Relational.EQ.setValue("qkm")>it means v.name == "qkm".</p>
     * <p>if you represents a logic relationship you can pass in
     * <"name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),Relational.EQ.setValue("SC"))>
     * it means v.name == "qkm" or v.name == "SC".</p>
     * <p>all map elements represents an and logical relationship.</p>
     *
     * @param conMap       String is propName,Relational is {@link Filter}
     *                     include (Relational、Logical、UnaryOperate)
     * @param filterString filterString is alternative ,you can pass in
     *                     "e.name == "qkm"" or "v.name == "qkm"" or "v1.name == "qkm""
     *                     or front same to pass in <"name",Relational.EQ.setValue("qkm")>
     *                     for conMap TODO check format
     * @return RelationshipMatch
     */
    public RelationshipMatch where(HashMap<String, Filter> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return this;
    }

    /**
     * if pass in negative means from 0 start.
     *
     * @param skip return from line skip of the result
     * @return RelationshipMatch
     */
    public RelationshipMatch skip(long skip) {
        this.skip = skip;
        return this;
    }

    /**
     * <p>if order by and group by are used together,the field of orderBy passed must be
     * included in {@link #groupBy(List, List)}groupBy and aggregateFunctions,
     * this function will sort directly using the passed alias,you can pass in
     * <{@link AttributeColumn}(null,alias),{@link Sort}>.</p>
     * <p>if you only use order by,this function puts the orderBy field you passed in
     * after the return field as output,and then sorts the aliases,you must pass in
     * <{@link AttributeColumn}("propName",alias),{@link Sort}>.</p>
     * <p>if you do not use order by, return e.</p>
     * <p>order by uses an alias to sort, so you must pass in an alias,
     * if the sort you passed is null,default is ASC.</p>
     *
     * @param orderBy sort by one or multiple attribute.</p>
     * @return RelationshipMatch
     */
    public RelationshipMatch orderBy(HashMap<AttributeColumn, Sort> orderBy) {
        this.orderBy = orderBy;
        return this;
    }

    /**
     * achieve group by use aggregateFunctions.eg:return v.name [as name],max(v.age) [as max].
     *
     * <p>if group by and order by are used together,for the field you need order by,
     * you must pass in alias.eg:{@link AttributeColumn}(propName,alias) or
     * {@link FunctionColumn}(AggregateFunction,alias).</p>
     * <p>if you only use group by,the alias in the column is optional,
     * you can only pass the {@link AttributeColumn}(propName, null) or
     * {@link FunctionColumn}(AggregateFunction,null).</p>
     *
     * @param groupBy            for grouping
     * @param aggregateFunctions for calculatio
     * @return RelationshipMatch
     */
    public RelationshipMatch groupBy(List<AttributeColumn> groupBy, List<FunctionColumn> aggregateFunctions) {
        this.groupBy = groupBy;
        this.aggregateFunctions = aggregateFunctions;
        return this;
    }

    /**
     * if you pass in negative means show all results.
     *
     * @param limit start from line 0 of the default value and end with line limit,
     *              it can be used in combination with skip,skip can be change default start value.
     * @return RelationshipMatch
     */
    public RelationshipMatch limit(long limit) {
        this.limit = limit;
        return this;
    }

    /**
     * @return from result get the first relationship
     */
    public ResultSet.Record first() {
        if (all().rowsSize() != 0) {
            return all().rowValues(0);
        }
        return null;
    }

    /**
     * connect parameters.
     *
     * @return sentence
     */
    private String connectParameters() {
        StringBuilder result = new StringBuilder();
        if (edgeDirection.toString().equals("OUT")) {
            result.append(String.format("MATCH (v%s)-[%s]->(v1%s) ",
                Match.joinTag(startTagName, startTagMap), Match.joinEdge(edgeMap, edges),
                Match.joinTag(endTagName, endTagMap)));
        } else if (edgeDirection.toString().equals("IN")) {
            result.append(String.format("MATCH (v%s)<-[%s]-(v1%s) ",
                Match.joinTag(startTagName, startTagMap), Match.joinEdge(edgeMap, edges),
                Match.joinTag(endTagName, endTagMap)));
        } else {
            result.append(String.format("MATCH (v%s)-[%s]-(v1%s) ",
                Match.joinTag(startTagName, startTagMap), Match.joinEdge(edgeMap, edges),
                Match.joinTag(endTagName, endTagMap)));
        }
        result.append(Match.judgeAndJoinWhere(conMap, filterString, 1));
        result.append(Match.joinGroupByAndOrderBy(groupBy, aggregateFunctions, orderBy, 1));
        result.append(Match.joinSkipAndLimit(skip, limit));
        return result.toString();
    }

    /**
     * @return all qualified relationships
     */
    public ResultSet all() {
        String matchRelationship = connectParameters();
        ResultSet resultSet = graph.run(matchRelationship);
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
        return all().rowsSize();
    }

    /**
     * is there data that meets the conditions.
     *
     * @return true or false
     */
    public boolean exist() {
        return count() > 0;
    }
}
