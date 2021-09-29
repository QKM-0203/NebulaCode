/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.Relationship;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.ngqlbuilder.match.RelationshipMatch;
import com.vesoft.nebula.ngqlbuilder.match.RelationshipMatcher;
import com.vesoft.nebula.ngqlbuilder.operator.AggregateFunction;
import com.vesoft.nebula.ngqlbuilder.operator.EdgeDirection;
import com.vesoft.nebula.ngqlbuilder.operator.Sort;
import com.vesoft.nebula.ngqlbuilder.query.ngql.Column;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestMatchRelationship extends TestDataBase {
    {
        graph.createTag(hobby);
        graph.createTag(person);
        graph.createEdge(subject);
        graph.createEdge(work);
        graph.create(vertexOne);
        graph.create(vertexTwo);
        graph.create(vertexThird);
        graph.create(vertexFour);
        graph.create(relationship12);
        graph.create(relationship32);
        graph.create(relationship24);
        graph.create(relationship34);
        graph.create(relationship23);
        graph.create(relationship13);
        HashMap<String, Integer> indexOnsubject = new HashMap<>();
        graph.createEdgeIndex("work", "i_work", null);
        graph.createEdgeIndex("subject", "i_subject", null);
        indexOnsubject.put("teacherName", 10);
        graph.createEdgeIndex("subject", "i_subject_teacherName", indexOnsubject);
        indexOnsubject.put("object", 10);
        graph.createEdgeIndex("subject", "i_subject_teacherName_object", indexOnsubject);
        indexOnsubject.remove("teacherName", 10);
        graph.createEdgeIndex("subject", "i_subject_object", indexOnsubject);
        graph.run("REBUILD EDGE INDEX i_work, i_subject, i_subject_teacherName, i_subject_object, "
            + "i_subject_teacherName_object");
        graph.createTagIndex("hobby", "i_hobby", null);
        HashMap<String, Integer> indexOnperson = new HashMap<>();
        graph.createTagIndex("person", "i_person", null);
        indexOnperson.put("name", 10);
        graph.createTagIndex("person", "i_person_name", indexOnperson);
        indexOnperson.put("age", null);
        graph.createTagIndex("person", "i_person_name_age", indexOnperson);
        indexOnperson.remove("name", 10);
        graph.createTagIndex("person", "i_person_age", indexOnperson);
        graph.run("REBUILD TAG INDEX i_hobby, i_person_name, i_person, i_person_name_age, "
            + "i_person_age");
    }

    @Test
    public void testMatchBothDirectionByEdgeName() {
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        RelationshipMatch work1 = relationshipMatcher.match(null, null, null, null,
            EdgeDirection.BOTH, null, "work");
        ResultSet work = work1.all();
        List<ValueWrapper> edges = work.colValues("e");
        Relationship relationship = edges.get(0).asRelationship();
        assert work1.exist();
        assert work1.count() == 6;
        assert relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 3;
        Relationship relationship1 = edges.get(1).asRelationship();
        assert relationship1.srcId().asLong() == 3
            && relationship1.dstId().asLong() == 2;
        Relationship relationship2 = edges.get(2).asRelationship();
        assert relationship2.srcId().asLong() == 3
            && relationship2.dstId().asLong() == 4;
        Relationship relationship3 = edges.get(3).asRelationship();
        assert relationship3.srcId().asLong() == 1
            && relationship3.dstId().asLong() == 3;
        Relationship relationship4 = edges.get(4).asRelationship();
        assert relationship4.srcId().asLong() == 3
            && relationship4.dstId().asLong() == 2;
        Relationship relationship5 = edges.get(5).asRelationship();
        assert relationship5.srcId().asLong() == 3
            && relationship5.dstId().asLong() == 4;
    }

    @Test
    public void testMatchOutDirectionByEdge() throws UnsupportedEncodingException {
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        HashMap<String, Object> edgeBysubject = new HashMap<>();
        edgeBysubject.put("teacherName", "qkm");
        RelationshipMatch subject = relationshipMatcher.match(null, null, null, null,
            EdgeDirection.OUT, edgeBysubject, "subject");
        ResultSet work = subject.all();
        List<ValueWrapper> edges = work.colValues("e");
        Relationship relationship = edges.get(0).asRelationship();
        assert subject.exist();
        assert subject.count() == 2;
        assert relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 2
            && relationship.properties().get("object").asString().equals("math");
        Relationship relationship1 = edges.get(1).asRelationship();
        assert relationship1.srcId().asLong() == 2
            && relationship1.dstId().asLong() == 3
            && relationship1.properties().get("object").asString().equals("math");
    }

    @Test
    public void testMatchByEdgeNameAddWhere() {
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        RelationshipMatch subject = relationshipMatcher.match(null, null, null, null,
            EdgeDirection.OUT, null, "subject");
        ResultSet work = subject.where(null, "e.teacherName == \"sy\"").all();
        assert !subject.exist();
        assert subject.count() == 0;
    }

    @Test
    public void testMatchByEdgeNameAddSrcTagNameAddWhere() throws UnsupportedEncodingException {
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        RelationshipMatch match = relationshipMatcher.match("person", null, null, null,
            EdgeDirection.OUT, null, "subject");
        ResultSet work = match.where(null, "v.name == \"sc\"").all();
        List<ValueWrapper> edges = work.colValues("e");
        Relationship relationship = edges.get(0).asRelationship();
        assert match.exist();
        assert match.count() == 2;
        assert relationship.srcId().asLong() == 2
            && relationship.dstId().asLong() == 3
            && relationship.properties().get("object").asString().equals("math");
        Relationship relationship1 = edges.get(1).asRelationship();
        assert relationship1.srcId().asLong() == 2
            && relationship1.dstId().asLong() == 4
            && relationship1.properties()
            .get("object").asString().equals("chinese");
    }

    @Test
    public void testMatchByEdgeNameAddSrcTagAddWhere()
        throws UnsupportedEncodingException {
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        HashMap<String, Object> tag = new HashMap<>();
        tag.put("name", "qkm");
        RelationshipMatch match = relationshipMatcher.match("person", tag, null, null,
            EdgeDirection.OUT, null, "subject");
        ResultSet work = match.where(null, "v.age == 19").all();
        List<ValueWrapper> edges = work.colValues("e");
        Relationship relationship = edges.get(0).asRelationship();
        assert match.exist();
        assert match.count() == 1;
        assert relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 2
            && relationship.properties().get("object").asString().equals("math");
    }

    @Test
    public void testMatchByEdgeNameAddSrcTagNameAddDstTagNameAddWhere()
        throws UnsupportedEncodingException {
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        RelationshipMatch match = relationshipMatcher.match("person", null, "hobby", null,
            EdgeDirection.OUT, null, "subject");
        ResultSet work = match.where(null, "v.name == \"sc\" and v1.name == \"sy\"").all();
        List<ValueWrapper> edges = work.colValues("e");
        Relationship relationship = edges.get(0).asRelationship();
        assert match.exist();
        assert match.count() == 1;
        assert relationship.srcId().asLong() == 2
            && relationship.dstId().asLong() == 3
            && relationship.properties().get("object").asString().equals("math");
    }

    @Test
    public void testMatchMultipleEdgeNameAddWhere() throws UnsupportedEncodingException {
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        RelationshipMatch match = relationshipMatcher.match("person", null, null, null,
            EdgeDirection.OUT, null, "subject", "work");
        ResultSet work = match.where(null, "v.name == \"qkm\"").all();
        List<ValueWrapper> edges = work.colValues("e");
        Relationship relationship = edges.get(0).asRelationship();
        assert match.exist();
        assert match.count() == 2;
        assert relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 2
            && relationship.properties().get("object").asString().equals("math");
        Relationship relationship1 = edges.get(1).asRelationship();
        assert relationship1.srcId().asLong() == 1
            && relationship1.dstId().asLong() == 3
            && relationship1.edgeName().equals("work");
    }

    @Test
    public void testMatchAddOrderBy() throws UnsupportedEncodingException {
        HashMap<Column, Sort> orderBy = new HashMap<>();
        Column column = new Column("v.name", "name");
        orderBy.put(column, Sort.DESC);
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        RelationshipMatch match = relationshipMatcher.match("hobby", null, null, null,
            EdgeDirection.OUT, null, "subject");
        ResultSet work = match.where(null).orderBy(orderBy, null, null).all();
        List<ValueWrapper> edges = work.colValues("name");
        assert match.exist();
        assert match.count() == 3;
        assert edges.get(0).asString().equals("sc");
        assert edges.get(1).asString().equals("sc");
        assert edges.get(2).asString().equals("qkm");
    }

    @Test
    public void testMatchOrderByAddGroupBy() throws UnsupportedEncodingException {
        HashMap<Column, Sort> orderBy = new HashMap<>();
        Column column = new Column("e.teacherName", "name");
        orderBy.put(column, Sort.DESC);
        List<Column> groupBy = new ArrayList<>();
        List<Column> aggregateFunctions = new ArrayList<>();
        groupBy.add(column);
        Column aggregateColumn = new Column(AggregateFunction.COUNT.setValue("*"), "count");
        aggregateFunctions.add(aggregateColumn);
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        RelationshipMatch match = relationshipMatcher.match("hobby", null, null, null,
            EdgeDirection.OUT, null, "subject");
        ResultSet work = match.where(null).orderBy(orderBy, null, null)
            .groupBy(groupBy, aggregateFunctions).all();
        assert match.exist();
        assert match.count() == 2;
        List<ValueWrapper> outPut = work.colValues("name");
        assert outPut.get(0).asString().equals("sc");
        assert outPut.get(1).asString().equals("qkm");
        List<ValueWrapper> outPut1 = work.colValues("count");
        assert outPut1.get(0).asLong() == 1;
        assert outPut1.get(1).asLong() == 2;
    }
}
