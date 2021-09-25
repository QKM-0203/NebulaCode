/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.operator.EdgeDirection;
import com.vesoft.nebula.orm.query.util.KeyWord;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * <p>you can use {@link #where(Map, String...)} for conditional filtering,
 * and use {@link #skip(long)}, {@link #limit(long)}, {@link #groupBy(List, List)}}
 * and  to operate the output results.</p>
 * <p>the user does not need to consider the calling order,
 * when the user calls the {@link #all()}、{@link #first()} etc method,
 * the parameter connection will be made.</p>
 * <p>note: make sure that at least one index is available for the match statement.</p>
 *
 * @author Qi Kai Meng
 */
public class RelationshipMatch extends Match<RelationshipMatch> {
    private String startTagName;
    private String endTagName;
    private List<String> edges;
    private EdgeDirection edgeDirection = EdgeDirection.OUT;
    private Map<String, Object> startTagMap;
    private Map<String, Object> endTagMap;
    private Map<String, Object> edgeMap;

    protected RelationshipMatch(Graph graph) {
        super(graph);
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
    protected RelationshipMatch init(String startTagName, Map<String, Object> startTagMap,
                                     String endTagName, Map<String, Object> endTagMap,
                                     EdgeDirection edgeDirection, Map<String, Object> edgeMap,
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
     * connect parameters.
     *
     * @return sentence
     */
    public String connectParameters() {
        StringBuilder result = new StringBuilder();
        if (edgeDirection.toString().equals("OUT")) {
            result.append(String.format(KeyWord.MATCH + " (v%s)-[%s]->(v1%s)",
                joinTag(startTagName, startTagMap), joinEdge(edgeMap, edges),
                joinTag(endTagName, endTagMap)));
        } else if (edgeDirection.toString().equals("IN")) {
            result.append(String.format(KeyWord.MATCH + " (v%s)<-[%s]-(v1%s)",
                joinTag(startTagName, startTagMap), joinEdge(edgeMap, edges),
                joinTag(endTagName, endTagMap)));
        } else {
            result.append(String.format(KeyWord.MATCH + " (v%s)-[%s]-(v1%s)",
                joinTag(startTagName, startTagMap), joinEdge(edgeMap, edges),
                joinTag(endTagName, endTagMap)));
        }
        result.append(judgeAndJoinWhere(conMap, filterString, 1));
        result.append(joinGroupByAndOrderBy(groupBy, aggregateFunctions, orderByMatch, 1));
        result.append(joinSkipAndLimit(skip, limit));
        System.out.println(result);
        return result.toString().trim();
    }
}
