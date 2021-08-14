/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.Vertex;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * match vertex
 */
public class VertexMatcher extends VertexMatch {

    public VertexMatcher(Graph graph) {
        super(graph);
    }

    /**
     * @param tagName tagName
     * @param propMap eg: query match (v:player{name: "qkm"}),if you create index on tag,
     *                you can pass in propMap
     * @return VertexMatch
     */
    public VertexMatch match(String tagName, HashMap<String, Object> propMap) {
        return init(tagName, propMap);
    }

    /**
     * getVertex by vid
     *
     * @param vid one vid
     * @return one vertex
     */
    public Vertex getVertexByVid(Object vid) {
        return match(null, null).where(null, "id(v) = " + (vid instanceof String
            ? "\"" + vid + "\"" : vid.toString())).first();
    }

    /**
     * getVertex by vidList
     *
     * @param vidList vidList
     * @return vertexList
     */
    public List<Vertex> getVertexByListVid(List<Object> vidList) {
        ArrayList<String> revertVid = new ArrayList<>();
        for (Object id : vidList) {
            if (id instanceof String) {
                revertVid.add("\"" + id + "\"");
            } else {
                revertVid.add(id.toString());
            }
        }
        return match(null, null).where(null, "id(v) IN " + revertVid).all();
    }
}
