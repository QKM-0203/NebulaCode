/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.match;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.ngqlbuilder.entity.Graph;
import com.vesoft.nebula.ngqlbuilder.query.cypher.Encoding;
import java.util.List;
import java.util.Map;

/**
 * the user gets the {@link VertexMatcher} object by passing the {@link Graph} object,
 * and user can use {@link #match(String, Map)} method by pass in about tag,use
 * {@link #getVertexByVid(Object)} method by pass in vid get Vertex and use
 * {@link #getVertexByVid(List)} method by pass in vidList get Vertexes.
 *
 * @author Qi Kai Meng
 */
public class VertexMatcher {
    private final VertexMatch vertexMatch;

    public VertexMatcher(Graph graph) {
        this.vertexMatch = new VertexMatch(graph);
    }

    /**
     * @param tagName if vertex has tag index,you can pass in,eg:
     *                match (v:player),can be null eg: match (v).
     * @param propMap if you create tag prop index,you can pass in propMap
     *                eg: match (v:player{name: "qkm"}),can be null eg:
     *                match (v:player).
     * @return VertexMatch
     */
    public VertexMatch match(String tagName, Map<String, Object> propMap) {
        return vertexMatch.init(tagName, propMap);
    }

    /**
     * getVertex by vid eg:match (v) where id(v) == vid.
     *
     * @param vid one vid
     * @return one vertex
     */
    public ResultSet.Record getVertexByVid(Object vid) {
        return match(null, null).where(null, "id(v) == " + (vid instanceof String
            ? "\"" + vid + "\"" : vid.toString())).first();
    }

    /**
     * getVertex by vidList eg:match (v) where id(v) IN [vid1,vid2,vid3].
     *
     * @param vidList vidList
     * @return vertexList
     */
    public ResultSet getVertexByVid(List<?> vidList) {
        return match(null, null)
            .where(null, String.format("id(v) IN [%s]", Encoding.encodeIdList(vidList))).all();
    }
}
