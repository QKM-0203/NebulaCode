/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.operator.EdgeDirection;
import java.util.HashMap;

public class RelationshipMatcher extends RelationshipMatch {

    public RelationshipMatcher(Graph graph) {
        super(graph);
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
    public RelationshipMatch match(String startTagName, HashMap<String, Object> startTagMap,
                                   String endTagName, HashMap<String, Object> endTagMap,
                                   EdgeDirection edgeDirection, HashMap<String, Object> edgeMap,
                                   String... types) {
        return init(startTagName, startTagMap, endTagName, endTagMap, edgeDirection, edgeMap, types);
    }
}
