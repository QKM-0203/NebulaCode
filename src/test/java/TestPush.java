/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.entity.Vertex;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import org.junit.Test;

public class TestPush extends TestDataBase {
    @Test
    public void testPushVertex() throws UnsupportedEncodingException {
        HashMap<String, HashMap<String, Object>> map = new HashMap<>();
        map.put("QKM3", null);
        Vertex vertex = new Vertex(5, map);
        graph.create(vertex);
        vertex.setVid(vertexOne.getVid());
        graph.push(vertexOne);
        graph.pull(vertex);
        assert vertex.getTagNames().equals(vertexOne.getTagNames());
    }

    @Test
    public void testPushRelationship() throws UnsupportedEncodingException {
        Relationship relationship = new Relationship(2, 3, "team", null, 1);
        graph.create(relationship);
        relationship.setStartVid(relationship12.getStartVid());
        relationship.setEndVid(relationship12.getEndVid());
        graph.push(relationship12);
        graph.pull(relationship);
        assert relationship12.properties().equals(relationship.properties());
    }

    @Test
    public void testPushVertexValueFullCoverage() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        vertexValueOne.remove("age", 19);
        vertexValueOne.put("name", "QKM");
        graph.push(vertexOne);
        graph.pull(vertexOne);
        assert vertexOne.getTag("QKM2").get("name").equals("QKM");
        assert vertexOne.getTag("QKM2").get("age") == null;
    }

    @Test
    public void testPushRelationshipValue() throws UnsupportedEncodingException {
        graph.create(relationship12);
        relationshipValueOne.put("teacherName", "SC");
        graph.push(relationship12);
        graph.pull(relationship12);
        assert relationship12.properties().get("teacherName").equals("SC");
    }

    public void assertEdgePropValue(Relationship relationship, String propName, String value) throws UnsupportedEncodingException {
        ResultSet resultSet = graph.run(String.format("FETCH PROP ON `%s` %s->%s@%d",
            relationship.getEdgeName(), relationship.getStartVid() instanceof String
                ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
            relationship.getEndVid() instanceof String
                ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
            relationship.getRank()));
        assert resultSet.colValues("edges_")
            .get(0).asRelationship().properties().get(propName).asString().equals(value);

    }

    public void assertTagPropValue(String tagName, String vid, String propName, String value) throws UnsupportedEncodingException {

        ResultSet resultSet = graph.run("FETCH PROP ON * " + vid);
        assert resultSet.colValues("vertices_")
            .get(0).asNode().properties(tagName).get(propName).asString().equals(value);

    }

    @Test
    public void testPushSubgraph() throws UnsupportedEncodingException {
        graph.create(subgraph);
        relationshipValueOne.put("teacherName", "SC");
        vertexValueOne.put("name", "QKM");
        graph.push(subgraph);
        graph.pull(subgraph);
        assertEdgePropValue(relationship12, "teacherName", "SC");
        assertTagPropValue("QKM2", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString(), "name", "QKM");
    }

    @Test
    public void testAddLabel() throws UnsupportedEncodingException {
        HashMap<String, HashMap<String, Object>> map = new HashMap<>();
        map.put("QKM3", null);
        Vertex vertex = new Vertex(5, map);
        graph.create(vertex);
        vertex.addTag("QKM4", null);
        graph.push(vertex);
        graph.pull(vertex);
        assert vertex.hasTag("QKM4");
    }

    @Test
    public void testSameLabel() throws UnsupportedEncodingException {
        HashMap<String, HashMap<String, Object>> map = new HashMap<>();
        map.put("QKM3", null);
        Vertex vertex = new Vertex(5, map);
        graph.create(vertex);
        assert vertex.hasTag("QKM3");
        graph.push(vertex);
        graph.pull(vertex);
        assert vertex.hasTag("QKM3");
    }

    @Test
    public void testDeleteLabel() throws UnsupportedEncodingException {
        HashMap<String, HashMap<String, Object>> map = new HashMap<>();
        map.put("QKM3", null);
        Vertex vertex = new Vertex(5, map);
        graph.create(vertex);
        map.put("QKM4", null);
        map.remove("QKM3", null);
        graph.push(vertex);
        graph.pull(vertex);
        assert !vertex.hasTag("QKM3");
    }


}
