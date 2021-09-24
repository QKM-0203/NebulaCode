/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.Relationship;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.orm.match.RelationshipMatch;
import com.vesoft.nebula.orm.match.RelationshipMatcher;
import com.vesoft.nebula.orm.match.VertexMatch;
import com.vesoft.nebula.orm.match.VertexMatcher;
import com.vesoft.nebula.orm.operator.AggregateFunction;
import com.vesoft.nebula.orm.operator.EdgeDirection;
import com.vesoft.nebula.orm.operator.Sort;
import com.vesoft.nebula.orm.query.ngql.Column;

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
        graph.createTag(qkm1);
        graph.createTag(qkm2);
        graph.createEdge(team);
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
        HashMap<String, Integer> indexOnTeam = new HashMap<>();
        graph.createEdgeIndex("work", "i_work", null);
        graph.createEdgeIndex("team", "i_team", null);
        indexOnTeam.put("teacherName", 10);
        graph.createEdgeIndex("team", "i_team_teacherName", indexOnTeam);
        indexOnTeam.put("object", 10);
        graph.createEdgeIndex("team", "i_team_teacherName_object", indexOnTeam);
        indexOnTeam.remove("teacherName", 10);
        graph.createEdgeIndex("team", "i_team_object", indexOnTeam);
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        graph.run("REBUILD EDGE INDEX i_work, i_team, i_team_teacherName, i_team_object, "
            + "i_team_teacherName_object");
        graph.createTagIndex("QKM1", "i_QKM1", null);
        HashMap<String, Integer> indexOnQKM2 = new HashMap<>();
        graph.createTagIndex("QKM2", "i_QKM2", null);
        indexOnQKM2.put("name", 10);
        graph.createTagIndex("QKM2", "i_QKM2_name", indexOnQKM2);
        indexOnQKM2.put("age", null);
        graph.createTagIndex("QKM2", "i_QKM2_name_age", indexOnQKM2);
        indexOnQKM2.remove("name", 10);
        graph.createTagIndex("QKM2", "i_QKM2_age", indexOnQKM2);
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        graph.run("REBUILD TAG INDEX i_QKM1, i_QKM2_name, i_QKM2, i_QKM2_name_age, "
            + "i_QKM2_age");
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
        HashMap<String, Object> edgeByTeam = new HashMap<>();
        edgeByTeam.put("teacherName", "qkm");
        RelationshipMatch team = relationshipMatcher.match(null, null, null, null,
            EdgeDirection.OUT, edgeByTeam, "team");
        ResultSet work = team.all();
        List<ValueWrapper> edges = work.colValues("e");
        Relationship relationship = edges.get(0).asRelationship();
        assert team.exist();
        assert team.count() == 2;
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
        RelationshipMatch team = relationshipMatcher.match(null, null, null, null,
            EdgeDirection.OUT, null, "team");
        ResultSet work = team.where(null, "e.teacherName == \"sy\"").all();
        assert !team.exist();
        assert team.count() == 0;
    }

    @Test
    public void testMatchByEdgeNameAddSrcTagNameAddWhere() throws UnsupportedEncodingException {
        RelationshipMatcher relationshipMatcher = new RelationshipMatcher(graph);
        RelationshipMatch match = relationshipMatcher.match("QKM2", null, null, null,
            EdgeDirection.OUT, null, "team");
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
        RelationshipMatch match = relationshipMatcher.match("QKM2", tag, null, null,
            EdgeDirection.OUT, null, "team");
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
        RelationshipMatch match = relationshipMatcher.match("QKM2", null, "QKM1", null,
            EdgeDirection.OUT, null, "team");
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
        RelationshipMatch match = relationshipMatcher.match("QKM2", null, null, null,
            EdgeDirection.OUT, null, "team", "work");
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
        RelationshipMatch match = relationshipMatcher.match("QKM1", null, null, null,
            EdgeDirection.OUT, null, "team");
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
        RelationshipMatch match = relationshipMatcher.match("QKM1", null, null, null,
            EdgeDirection.OUT, null, "team");
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
