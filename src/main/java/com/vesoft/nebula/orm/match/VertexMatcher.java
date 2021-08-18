/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.ngql.Encoding;
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
    public ResultSet.Record getVertexByVid(Object vid) {
        return match(null, null).where(null, "id(v) == " + (vid instanceof String
            ? "\"" + vid + "\"" : vid.toString())).first();
    }

    /**
     * getVertex by vidList
     *
     * @param vidList vidList
     * @return vertexList
     */
    public ResultSet getVertexByListVid(List<?> vidList) {
        return match(null, null)
            .where(null, String.format("id(v) IN [%s]", Encoding.encodeIdList(vidList))).all();
    }
}
