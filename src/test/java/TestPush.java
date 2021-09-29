/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.ngqlbuilder.entity.Property;
import com.vesoft.nebula.ngqlbuilder.entity.Relationship;
import com.vesoft.nebula.ngqlbuilder.entity.Schema;
import com.vesoft.nebula.ngqlbuilder.entity.Vertex;
import com.vesoft.nebula.ngqlbuilder.operator.DataType;
import com.vesoft.nebula.ngqlbuilder.timetype.DateTime;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestPush extends TestDataBase {
    {
        graph.createTag(person);
        graph.createTag(hobby);
        graph.createEdge(subject);
        graph.createEdge(work);
    }

    @Test
    public void testPushVertex() throws UnsupportedEncodingException {
        Schema qkm5 = new Schema("QKM5", null);
        graph.createTag(qkm5);
        HashMap<String, HashMap<String, Object>> map = new HashMap<>();
        map.put("QKM5", null);
        Vertex vertex5 = new Vertex(5, map);
        HashMap<String, HashMap<String, Object>> map1 = new HashMap<>();
        map1.put("QKM5", null);
        map1.put("QKM6", null);
        Vertex vertex6 = new Vertex(6, map1);
        graph.create(vertex5);
        vertex6.setVid(vertex5.getVid());
        graph.push(vertex6);
        graph.pull(vertex5);
        assert vertex5.getTagNames().equals(vertex6.getTagNames());
    }

    @Test
    public void testPushRelationship() {
        Property propertyOne = new Property("year", DataType.DATETIME, true, null);
        ArrayList<Property> properties = new ArrayList<>();
        properties.add(propertyOne);
        Schema qkm9 = new Schema("QKM9", properties);
        graph.createEdge(qkm9);
        HashMap<String, Object> map = new HashMap<>();
        DateTime dateTime = new DateTime("2002-02-03T12:30:12.000000");
        map.put("year",dateTime);
        Relationship relationship23 = new Relationship(2, 3, "QKM9", null, 1);
        Relationship relationship34 = new Relationship(4, 5, "QKM9", map, 0);
        graph.create(relationship23);
        relationship34.setStartVid(relationship23.getStartVid());
        relationship34.setEndVid(relationship23.getEndVid());
        relationship34.setEdgeName(relationship23.getEdgeName());
        try {
            graph.push(relationship34);
        } catch (Exception e) {
            assert e.getMessage().equals("the edge "
                + "(2)-[:QKM9@0{year: 2002-02-03T12:30:12.000000}]->(3) not exist");
        }
    }

    @Test
    public void testPushNotExistRelationship() throws UnsupportedEncodingException {
        Property propertyOne = new Property("year", DataType.DATETIME, true, null);
        ArrayList<Property> properties = new ArrayList<>();
        properties.add(propertyOne);
        Schema qkm9 = new Schema("QKM9", properties);
        graph.createEdge(qkm9);
        HashMap<String, Object> map = new HashMap<>();
        DateTime dateTime = new DateTime("2002-02-03T12:30:12.000000");
        map.put("year",dateTime);
        Relationship relationship23 = new Relationship(2, 3, "QKM9", null, 1);
        Relationship relationship34 = new Relationship(4, 5, "QKM9", map, 0);
        graph.create(relationship23);
        relationship34.setStartVid(relationship23.getStartVid());
        relationship34.setEndVid(relationship23.getEndVid());
        relationship34.setRank(relationship23.getRank());
        graph.push(relationship34);
        graph.pull(relationship23);
        assert relationship23.properties().get("year")
            .equals(relationship34.properties().get("year"));
        try {
            graph.push(relationship34);
        } catch (Exception e) {
            assert e.getMessage().equals("the edge (2)-[:QKM8@0{}]->(3) not exist");
        }
    }

    @Test
    public void testPushVertexValueFullCoverage() throws UnsupportedEncodingException {
        graph.create(vertexOne);
        qkmPersonValues.remove("age", 19);
        qkmPersonValues.put("name", "QKM");
        graph.push(vertexOne);
        graph.pull(vertexOne);
        assert vertexOne.getTag("person").get("name").equals("QKM");
        assert vertexOne.getTag("person").get("age") == null;
    }

    @Test
    public void testPushRelationshipValue() throws UnsupportedEncodingException {
        graph.create(relationship12);
        qkmSubjectValues.put("teacherName", "SC");
        graph.push(relationship12);
        graph.pull(relationship12);
        assert relationship12.properties().get("teacherName").equals("SC");
    }

    public void assertEdgePropValue(Relationship relationship, String propName, String value)
        throws UnsupportedEncodingException {
        ResultSet resultSet = graph.run(String.format("FETCH PROP ON `%s` %s->%s@%d",
            relationship.getEdgeName(), relationship.getStartVid() instanceof String
                ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
            relationship.getEndVid() instanceof String
                ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
            relationship.getRank()));
        assert resultSet.colValues("edges_")
            .get(0).asRelationship().properties().get(propName).asString().equals(value);

    }

    public void assertTagPropValue(String tagName, String vid, String propName, String value)
        throws UnsupportedEncodingException {

        ResultSet resultSet = graph.run("FETCH PROP ON * " + vid);
        assert resultSet.colValues("vertices_")
            .get(0).asNode().properties(tagName).get(propName).asString().equals(value);

    }

    @Test
    public void testPushSubgraph() throws UnsupportedEncodingException {
        graph.create(subgraph);
        qkmSubjectValues.put("teacherName", "SC");
        qkmPersonValues.put("name", "QKM");
        graph.push(subgraph);
        graph.pull(subgraph);
        assertEdgePropValue(relationship12, "teacherName", "SC");
        assertTagPropValue("person", vertexOne.getVid() instanceof String
            ? "\"" + vertexOne.getVid() + "\"" : vertexOne.getVid().toString(), "name", "QKM");
    }

    @Test
    public void testAddLabel() throws UnsupportedEncodingException {
        Schema qkm5 = new Schema("QKM5", null);
        Schema qkm6 = new Schema("QKM6", null);
        graph.createTag(qkm5);
        graph.createTag(qkm6);
        HashMap<String, HashMap<String, Object>> map = new HashMap<>();
        map.put("QKM5", null);
        Vertex vertex = new Vertex(5, map);
        graph.create(vertex);
        vertex.addTag("QKM6", null);
        graph.push(vertex);
        graph.pull(vertex);
        assert vertex.hasTag("QKM6");
    }

    @Test
    public void testSameLabel() throws UnsupportedEncodingException {
        Schema qkm5 = new Schema("QKM5", null);
        graph.createTag(qkm5);
        HashMap<String, HashMap<String, Object>> map = new HashMap<>();
        map.put("QKM5", null);
        Vertex vertex = new Vertex(5, map);
        graph.create(vertex);
        assert vertex.hasTag("QKM5");
        graph.push(vertex);
        graph.pull(vertex);
        assert vertex.hasTag("QKM5");
    }

    @Test
    public void testDeleteLabel() throws UnsupportedEncodingException {
        Schema qkm5 = new Schema("QKM5", null);
        Schema qkm6 = new Schema("QKM6", null);
        graph.createTag(qkm5);
        graph.createTag(qkm6);
        HashMap<String, HashMap<String, Object>> map = new HashMap<>();
        map.put("QKM5", null);
        Vertex vertex = new Vertex(5, map);
        graph.create(vertex);
        map.put("QKM6", null);
        map.remove("QKM5", null);
        graph.push(vertex);
        graph.pull(vertex);
        assert !vertex.hasTag("QKM5");
    }


}
