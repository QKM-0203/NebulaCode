/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.*;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import org.junit.Test;

/**
 * test merge
 */
public class TestMerge extends TestDataBase {
    @Test
    public void mergeVertexThatDoesNotExist() throws UnsupportedEncodingException {
        graph.delete(vertexOne);
        assert !graph.exists(vertexOne);
        graph.merge(vertexOne, "QKM2", "name");
        graph.exists(vertexOne);
    }

    @Test
    public void mergeVertexThatDoesExist() throws UnsupportedEncodingException, InterruptedException {
        graph.create(vertexOne);
        assert graph.exists(vertexOne);
        graph.run("submit job stats");
        Thread.sleep(1000);
        final long oldVertexNumber = graph.vertexNumber(null);
        graph.merge(vertexOne, "QKM2", "name");
        graph.run("submit job stats");
        Thread.sleep(1000);
        final long newVertexNumber = graph.vertexNumber(null);
        assert newVertexNumber == oldVertexNumber;
    }

    @Test
    public void mergeVertexThatUpdatePropValue() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        assert graph.exists(vertexOne);
        vertexValueOne.put("name", "QKM");
        graph.merge(vertexOne, "QKM2", "name");
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString());
    }

    @Test
    public void mergeVertexThatAddTag() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        assert graph.exists(vertexOne);
        HashMap<String, HashMap<String, Object>> vertexMapOneAddTag = new HashMap<>();
        Schema qkm7 = new Schema("QKM7");
        graph.createTag(qkm7);
        vertexMapOneAddTag.put("QKM7", null);
        Vertex vertexOneAddTag = new Vertex(vertexOne.getVid(), vertexMapOneAddTag);
        graph.merge(vertexOneAddTag, "QKM2", "age");
        graph.pull(vertexOne);
        assert vertexOne.hasTag("QKM7");

    }

    public void assertTagPropValue(String tagName, String vid) throws UnsupportedEncodingException {
        ResultSet resultSet = graph.run("FETCH PROP ON * " + vid);
        assert resultSet.colValues("vertices_")
            .get(0).asNode().properties(tagName).get("name").asString().equals("QKM");

    }

    @Test
    public void mergeRelationshipThatDoesNotExist() throws UnsupportedEncodingException {
        graph.delete(relationship12);
        assert !graph.exists(relationship12);
        graph.merge(relationship12);
        graph.exists(relationship12);
    }


    @Test
    public void mergeRelationshipThatDoesExist() throws UnsupportedEncodingException, InterruptedException {
        graph.create(relationship12);
        assert graph.exists(relationship12);
        graph.run("submit job stats");
        Thread.sleep(1000);
        final long oldEdgeNumber = graph.relationshipNumber(null);
        graph.merge(relationship12, "team", "teamName");
        graph.run("submit job stats");
        Thread.sleep(1000);
        final long newEdgeNumber = graph.relationshipNumber(null);
        assert oldEdgeNumber == newEdgeNumber;
    }

    @Test
    public void mergeRelationshipThatUpdatePropValue() throws UnsupportedEncodingException {
        graph.create(relationship12);
        assert graph.exists(relationship12);
        relationshipValueOne.put("teamName", "China");
        graph.merge(relationship12);
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
            .get(0).asRelationship().properties().get("teamName").asString().equals("China");

    }

    @Test
    public void mergeSubgraphThatThreeVertexDoesNotExist() throws UnsupportedEncodingException {
        graph.delete(vertexOne);
        graph.delete(vertexTwo);
        graph.delete(vertexThird);
        assert !graph.exists(vertexOne);
        assert !graph.exists(vertexOne);
        assert !graph.exists(vertexThird);
        graph.merge(subgraph, "QKM2", "name");
        assert graph.exists(vertexOne);
        assert graph.exists(vertexTwo);
        assert graph.exists(vertexThird);
    }

    @Test
    public void mergeSubgraphThatOneVertexDoesExist() throws UnsupportedEncodingException {
        graph.delete(vertexOne);
        graph.delete(vertexTwo);
        graph.delete(vertexThird);
        assert !graph.exists(vertexOne);
        assert !graph.exists(vertexOne);
        assert !graph.exists(vertexThird);
        graph.create(vertexOne);
        assert graph.exists(vertexOne);
        ArrayList<Vertex> vertices = new ArrayList<>();
        vertices.add(vertexOne);
        vertices.add(vertexTwo);
        vertices.add(vertexThird);
        Subgraph subgraph = new Subgraph(vertices);
        graph.merge(subgraph, "QKM2", "name");
        graph.exists(subgraph);
    }

    @Test
    public void mergePathThatOneVertexExist() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        assert graph.exists(vertexOne);
        graph.merge(path, "QKM2", "name");
        assert graph.exists(path);
    }

    @Test
    public void testParameterIsNull() {
        try {
            graph.merge(vertexOne, null, null);
        } catch (Exception e) {
            assert e.getMessage().equals("tagName and attribute name is"
                + " a condition of merge,so cannot be null");
        }
    }
}
