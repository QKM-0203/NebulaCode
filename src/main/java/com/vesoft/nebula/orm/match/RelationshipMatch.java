/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.ngql.Name;
import com.vesoft.nebula.orm.ngql.Query;
import com.vesoft.nebula.orm.operator.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class RelationshipMatch {
    protected Graph graph;
    private String startTagName;
    private String endTagName;
    private List<String> edges;
    private List<String> filterString;
    private long skip = 0;
    private long limit;
    private HashMap<Name, Sort> orderBy;
    private List<Name> groupBy;
    private EdgeDirection edgeDirection = EdgeDirection.OUT;
    private HashMap<String, Filter> conMap;
    private HashMap<String, Object> startTagMap;
    private HashMap<String, Object> endTagMap;
    private HashMap<String, Object> edgeMap;
    private List<AggregateFunction> aggregateFunctions;

    protected RelationshipMatch(Graph graph) {
        this.graph = graph;
    }

    /**
     * @param startTagName if tag of startVertex,you can pass in,can be null
     * @param startTagMap  if startVertex has tag index you can pass in,can be null
     * @param endTagName   if tag of endVertex,you can pass in,can be null
     * @param endTagMap    if endVertex has tag index you can pass in,can be null
     * @param edgeDirection    in edge or out edge
     * @param edgeMap      if you create edge index,you can pass in ,eg:
     *                     match (v)-[e:player{name: "qkm"}]-(v2)
     * @param types        edgeName,can be multiple
     * @return RelationshipMatch
     */
    public RelationshipMatch init(String startTagName, HashMap<String, Object> startTagMap,
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
        return this;
    }

    /**
     * filter condition
     *
     * <p>For conMap,if you represents a relationship ,you can pass in
     * <"name",Relational.EQ.setValue("qkm")>it means v.name == "qkm".</p>
     * <p>if you represents a logic relationship you can pass in
     * <"name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),Relational.EQ.setValue("SC"))>
     * it means v.name == "qkm" or v.name == "SC";</p>
     * <p>all map elements represents an and logical relationship</p>
     *
     * @param conMap       String is propName,Relational is {@link Filter}
     *                     include (Relational、Logical、UnaryOperate)
     * @param filterString filterString is alternative ,you can pass in
     *                     "e.name == "qkm"" or "v.name == "qkm"" or "v1.name == "qkm"",
     *                     or front same to pass in <"name",Relational.EQ.setValue("qkm")> for conMap.
     * @return RelationshipMatch
     */
    public RelationshipMatch where(HashMap<String, Filter> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return this;
    }

    /**
     * @param skip return from line skip of the result
     * @return RelationshipMatch
     */
    public RelationshipMatch skip(long skip) {
        this.skip = skip;
        return this;
    }

    /**
     * @param orderBy sort by one or multiple attribute,pass in eg: (e.name,Sort.ASC)
     * @return RelationshipMatch
     */
    public RelationshipMatch orderBy(HashMap<Name, Sort> orderBy) {
        this.orderBy = orderBy;
        return this;
    }

    /**
     * grouping using aggregate functions.
     *
     * @param groupBy            for grouping,{@link Name} Object is used to alias properties
     * @param aggregateFunctions for calculation
     * @return RelationshipMatch
     */
    public RelationshipMatch groupBy(List<Name> groupBy, AggregateFunction... aggregateFunctions) {
        this.groupBy = groupBy;
        this.aggregateFunctions = Arrays.asList(aggregateFunctions);
        return this;
    }

    /**
     * @param limit start from line 0 of the default value and end with line limit,
     *              it can be used in combination with skip,skip can be change default start value
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
     * connect parameters
     *
     * @return sentence
     */
    private String connectQueryParameters() {
        StringBuilder result = new StringBuilder();
        if (edgeDirection.toString().equals("OUT")) {
            result.append(String.format("MATCH (v%s)-[%s]->(v1%s) ",
                Query.joinTag(startTagName, startTagMap), Query.joinEdge(edgeMap, edges),
                Query.joinTag(endTagName, endTagMap)));
        } else if (edgeDirection.toString().equals("IN")) {
            result.append(String.format("MATCH (v%s)<-[%s]-(v1%s) ",
                Query.joinTag(startTagName, startTagMap), Query.joinEdge(edgeMap, edges),
                Query.joinTag(endTagName, endTagMap)));
        } else {
            result.append(String.format("MATCH (v%s)-[%s]-(v1%s) ",
                Query.joinTag(startTagName, startTagMap), Query.joinEdge(edgeMap, edges),
                Query.joinTag(endTagName, endTagMap)));
        }
        result.append(Query.judgeAndJoinWhere(conMap,filterString,1));
        result.append(Query.joinGroupByAndOrderBy(groupBy,aggregateFunctions,orderBy));
        result.append(Query.joinSkipAndLimit(skip,limit));
        return result.toString();
    }

    /**
     * @return all qualified relationships
     */
    public ResultSet all() {
        String matchRelationship = connectQueryParameters();
        ResultSet resultSet = graph.run(matchRelationship);
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
