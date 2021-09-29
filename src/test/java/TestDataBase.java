/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.ngqlbuilder.entity.*;
import com.vesoft.nebula.ngqlbuilder.operator.DataType;
import com.vesoft.nebula.ngqlbuilder.timetype.DateTime;
import java.util.*;

public class TestDataBase {
    public GraphService graphService = new GraphService(
        Arrays.asList(new HostAddress("127.0.0.1", 9669),
            new HostAddress("127.0.0.1", 9898)),
        "root", "nebula", false);
    public Graph graph = graphService.getGraph("Test");
    HashMap<String, HashMap<String, Object>> qkmTags = new HashMap<>();
    HashMap<String, HashMap<String, Object>> scTags = new HashMap<>();
    HashMap<String, HashMap<String, Object>> syTags = new HashMap<>();
    HashMap<String, HashMap<String, Object>> yqTags = new HashMap<>();
    HashMap<String, Object> qkmPersonValues = new HashMap<>();
    HashMap<String, Object> scPersonValues = new HashMap<>();
    HashMap<String, Object> syPersonValues = new HashMap<>();
    HashMap<String, Object> yqPersonValues = new HashMap<>();
    HashMap<String, Object> qkmSubjectValues = new HashMap<>();
    HashMap<String, Object> scSubjectValues = new HashMap<>();
    Vertex vertexOne;
    Vertex vertexTwo;
    Vertex vertexThird;
    Vertex vertexFour;
    Relationship relationship12;
    Relationship relationship32;
    Relationship relationship24;
    Relationship relationship34;
    Relationship relationship23;
    Relationship relationship13;
    Property name = new Property("name", DataType.STRING.setLength(10), true, null);
    Property age = new Property("age", DataType.INT, true, null);
    Property birth = new Property("birth", DataType.DATETIME, true, null);
    Property teacherName = new Property("teacherName", DataType.STRING.setLength(10), true, null);
    Property object = new Property("object", DataType.STRING.setLength(20), true, null);
    ArrayList<Property> personSchema = new ArrayList<>();
    ArrayList<Property> subjectSchema = new ArrayList<>();
    Schema hobby;
    Schema person;
    Schema subject;
    Schema work;
    Subgraph subgraph;
    Segment segment12;
    Segment segment32;
    Segment segment24;
    Segment segment34;
    Path path;

    {
        qkmPersonValues.put("name", "qkm");
        qkmPersonValues.put("age", 19);
        qkmPersonValues.put("birth", new DateTime("2002-02-03T06:12:12:123"));
        scPersonValues.put("name", "sc");
        scPersonValues.put("age", 19);
        scPersonValues.put("birth", new DateTime("2001-04-07T06:12:12:123"));
        syPersonValues.put("name", "sy");
        syPersonValues.put("age", 20);
        syPersonValues.put("birth", new DateTime("2001-08-13T06:12:12:123"));
        yqPersonValues.put("name", "yq");
        yqPersonValues.put("age", 21);
        yqPersonValues.put("birth", new DateTime("1999-12-25T06:12:12:123"));
        qkmTags.put("hobby", null);
        qkmTags.put("person", qkmPersonValues);
        scTags.put("hobby", null);
        scTags.put("person", scPersonValues);
        syTags.put("hobby", null);
        syTags.put("person", syPersonValues);
        yqTags.put("hobby", null);
        yqTags.put("person", yqPersonValues);
        vertexOne = new Vertex(1, qkmTags);
        vertexTwo = new Vertex(2, scTags);
        vertexThird = new Vertex(3, syTags);
        vertexFour = new Vertex(4, yqTags);
        qkmSubjectValues.put("teacherName", "qkm");
        qkmSubjectValues.put("object", "math");
        scSubjectValues.put("teacherName", "sc");
        scSubjectValues.put("object", "chinese");
        relationship12 = new Relationship(1, 2, "subject", qkmSubjectValues, 1);
        relationship32 = new Relationship(3, 2, "work", null, 1);
        relationship13 = new Relationship(1, 3, "work", null, 0);
        relationship24 = new Relationship(2, 4, "subject", scSubjectValues, 1);
        relationship34 = new Relationship(3, 4, "work", null, 1);
        relationship23 = new Relationship(2, 3, "subject", qkmSubjectValues, 0);
        personSchema.add(name);
        personSchema.add(age);
        personSchema.add(birth);
        subjectSchema.add(teacherName);
        subjectSchema.add(object);
        person = new Schema("person", personSchema, 0, null);
        hobby = new Schema("hobby", null);
        subject = new Schema("subject", subjectSchema);
        work = new Schema("work", null);
        ArrayList<Relationship> relationshipList = new ArrayList<>();
        relationshipList.add(relationship12);
        relationshipList.add(relationship32);
        ArrayList<Vertex> vertices = new ArrayList<>();
        vertices.add(vertexOne);
        vertices.add(vertexTwo);
        vertices.add(vertexThird);
        subgraph = new Subgraph(vertices, relationshipList);
        segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        segment32 = new Segment(vertexThird, relationship32, vertexTwo);
        segment24 = new Segment(vertexTwo, relationship24, vertexFour);
        segment34 = new Segment(vertexThird, relationship34, vertexFour);
        List<Segment> segments = new ArrayList<>();
        segments.add(segment12);
        segments.add(segment32);
        segments.add(segment34);
        segments.add(segment24);
        path = new Path(segments);
    }
}
