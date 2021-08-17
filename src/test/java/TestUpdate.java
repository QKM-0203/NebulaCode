/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.*;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class TestUpdate extends Data {
    @Test
    public void testAddTagForVertex() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        vertexMapOne.put("QKM3", null);
        vertexMapOne.put("QKM8", null);
        graph.push(vertexOne);
        assertHasTag("QKM3", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());

    }

    public void assertHasTag(String tagName, String vid) throws UnsupportedEncodingException {
        ResultSet resultSet = graph.run("FETCH PROP ON * " + vid);
        assert resultSet.colValues("vertices_")
            .get(0).asNode().hasLabel(tagName);

    }

    @Test
    public void testUpdateTagValue() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        vertexValueOne.put("name", "QKM");
        graph.push(vertexOne);
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());

    }

    public void assertTagPropValue(String tagName, String vid) throws UnsupportedEncodingException {

        ResultSet resultSet = graph.run("FETCH PROP ON * " + vid);
        assert resultSet.colValues("vertices_")
            .get(0).asNode().properties(tagName).get("name").toString().equals("\"QKM\"");

    }

    @Test
    public void testUpdateRelationship() throws UnsupportedEncodingException {
        graph.create(relationship12);
        relationshipValueOne.put("teamName", "SC");
        graph.push(relationship12);
        assertEdgePropValue(relationship12);
    }

    public void assertEdgePropValue(Relationship relationship) throws UnsupportedEncodingException {
        ResultSet resultSet = graph.run(String.format("FETCH PROP ON `%s` %s->%s@%d",
            relationship.getEdgeName(), relationship.getStartVid() instanceof String
                ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
            relationship.getEndVid() instanceof String
                ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
            relationship.getRank()));
        assert resultSet.colValues("edges_")
            .get(0).asRelationship().properties().get("teamName").toString().equals("\"SC\"");

    }

    @Test
    public void testUpdateSubgraph() throws UnsupportedEncodingException {
        graph.create(subgraph);
        relationshipValueOne.put("teamName", "SC");
        vertexValueOne.put("name", "QKM");
        graph.push(subgraph);
        assertEdgePropValue(relationship12);
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());
    }

    @Test
    public void testUpdatePath() throws UnsupportedEncodingException {
        graph.create(path);
        relationshipValueOne.put("teamName", "SC");
        vertexValueOne.put("name", "QKM");
        graph.push(path);
        assertEdgePropValue(relationship12);
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());
    }
}
