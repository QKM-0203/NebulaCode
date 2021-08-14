/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.Vertex;
import com.vesoft.nebula.orm.operator.Direction;
import java.util.HashMap;

public class RelationshipMatcher extends RelationshipMatch {

    public RelationshipMatcher(Graph graph) {
        super(graph);
    }

    /**
     * @param startVertex startVertex,can be null
     * @param endVertex   endVertex,can be null
     * @param direction   in edge or out edge
     * @param propMap     if you create edge index,you can pass in eg: player{name: "qkm"}
     * @param types       edgeName,can be Multiple
     * @return RelationshipMatch
     */
    public RelationshipMatch match(Vertex startVertex, Vertex endVertex,
                                   Direction direction, HashMap<String, Object> propMap,
                                   String... types) {
        return init(startVertex, endVertex, direction, propMap, types);
    }
}
