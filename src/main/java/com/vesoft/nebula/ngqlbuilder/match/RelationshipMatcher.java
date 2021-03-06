/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.match;

import com.vesoft.nebula.ngqlbuilder.entity.Graph;
import com.vesoft.nebula.ngqlbuilder.operator.EdgeDirection;
import java.util.Map;

/**
 * the user gets the {@link RelationshipMatcher} object by passing the {@link Graph} object,
 * and then calls the {@link #match(String, Map, String, Map, EdgeDirection, Map, String...)}
 * method to pass the parameters.
 *
 * @author Qi Kai Meng
 */
public class RelationshipMatcher {
    private final RelationshipMatch relationshipMatch;

    public RelationshipMatcher(Graph graph) {
        this.relationshipMatch = new RelationshipMatch(graph);
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
    public RelationshipMatch match(String startTagName, Map<String, Object> startTagMap,
                                   String endTagName, Map<String, Object> endTagMap,
                                   EdgeDirection edgeDirection, Map<String, Object> edgeMap,
                                   String... types) {
        return relationshipMatch.init(startTagName, startTagMap, endTagName, endTagMap, edgeDirection, edgeMap, types);
    }
}
