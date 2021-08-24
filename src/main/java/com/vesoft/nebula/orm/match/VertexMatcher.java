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
 * the user gets the {@link VertexMatcher} object by passing the {@link Graph} object,
 * and user can use {@link #match(String, HashMap)} method by pass in about tag,use
 * {@link #getVertexByVid(Object)} method by pass in vid get Vertex and use
 * {@link #getVertexByListVid(List)} method by pass in vidList get Vertexes.
 */
public class VertexMatcher extends VertexMatch {

    public VertexMatcher(Graph graph) {
        super(graph);
    }

    /**
     * @param tagName if vertex has tag index,you can pass in,eg:
     *                match (v:player),can be null eg: match (v).
     * @param propMap if you create tag prop index,you can pass in propMap
     *                eg: match (v:player{name: "qkm"}),can be null eg:
     *                match (v:player).
     * @return VertexMatch
     */
    public VertexMatch match(String tagName, HashMap<String, Object> propMap) {
        init(tagName, propMap);
        return this;
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
    public ResultSet getVertexByListVid(List<?> vidList) {
        return match(null, null)
            .where(null, String.format("id(v) IN [%s]", Encoding.encodeIdList(vidList))).all();
    }
}
