/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.orm.operator.*;
import com.vesoft.nebula.orm.query.ngql.Column;
import com.vesoft.nebula.orm.query.ngql.Go;
import com.vesoft.nebula.orm.query.ngql.Goer;

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
public class TestGo extends TestDataBase {
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
    }

    @Test
    public void testGoGetDstID() {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.all();
        assert go.exist();
        assert go.count() == 3;
        List<ValueWrapper> dstId = all.colValues("team._dst");
        assert dstId.get(0).asLong() == 2;
        assert dstId.get(1).asLong() == 3;
        assert dstId.get(2).asLong() == 4;
    }

    @Test
    public void testGoReverselyDirectionGetDstID() {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.direction(PathDirection.REVERSELY).all();
        assert go.exist();
        assert go.count() == 1;
        List<ValueWrapper> dstId = all.colValues("team._dst");
        assert dstId.get(0).asLong() == 1;
    }

    @Test
    public void testGoStepsGetDstID() {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.step(0, 2).all();
        assert go.exist();
        assert go.count() == 3;
        List<ValueWrapper> dstId = all.colValues("team._dst");
        assert dstId.get(0).asLong() == 2
            && dstId.get(1).asLong() == 3
            && dstId.get(2).asLong() == 4;
    }

    @Test
    public void testGoIllegalStepsGetDstID() {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.step(0, -1).all();
        assert go.exist();
        assert go.count() == 1;
        ResultSet all1 = go.step(-1, -1).all();
        assert go.exist();
        assert go.count() == 1;
        List<ValueWrapper> dstId = all.colValues("team._dst");
        List<ValueWrapper> dstId1 = all1.colValues("team._dst");
        assert dstId.get(0).asLong() == dstId1.get(0).asLong();
    }

    @Test
    public void testGoGetDstIDAddWhere() {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("$^.QKM2.name", Relational.EQ.setValue("qkm"));
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.where(filter).all();
        assert go.exist();
        assert go.count() == 1;
        List<ValueWrapper> dstId = all.colValues("team._dst");
        assert dstId.get(0).asLong() == 2;
    }

    @Test
    public void testGoGetDstIDAddWhere2() {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.where(null, "$$.QKM2.name == \"sc\"").all();
        assert go.exist();
        assert go.count() == 1;
        List<ValueWrapper> dstId = all.colValues("team._dst");
        assert dstId.get(0).asLong() == 2;
    }

    @Test
    public void testGoGetDstIDAddWhere3() {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.where(null, "team.teacherName == \"qkm\"").all();
        assert go.exist();
        assert go.count() == 2;
        List<ValueWrapper> dstId = all.colValues("team._dst");
        assert dstId.get(0).asLong() == 3;
        assert dstId.get(1).asLong() == 2;
    }

    @Test
    public void testGoGetDstIDAddWhereYield() throws UnsupportedEncodingException {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.where(null, "$$.QKM2.age > 19")
            .yield("team.object as object").all();
        assert go.exist();
        assert go.count() == 2;
        List<ValueWrapper> object = all.colValues("object");
        assert object.get(0).asString().equals("math");
        assert object.get(1).asString().equals("chinese");
    }

    @Test
    public void testGoYield() throws UnsupportedEncodingException {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.yield("team.object as object").all();
        assert go.exist();
        assert go.count() == 3;
        List<ValueWrapper> object = all.colValues("object");
        assert object.get(0).asString().equals("math")
            && object.get(1).asString().equals("chinese")
            && object.get(2).asString().equals("math");
    }

    @Test
    public void testGoYieldAddDistinct() throws UnsupportedEncodingException {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.yieldWithDistinct("team.object as object").all();
        assert go.exist();
        assert go.count() == 2;
        List<ValueWrapper> object = all.colValues("object");
        assert object.get(0).asString().equals("math")
            && object.get(1).asString().equals("chinese");
    }

    @Test
    public void testGoGetDstIDAddWhereYieldAddOrderBy() throws UnsupportedEncodingException {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        Goer goer = new Goer(graph);
        HashMap<String, Sort> orderBy = new HashMap<>();
        orderBy.put("object", null);
        Go go = goer.go(ids, edges);
        ResultSet all = go.where(null, "$$.QKM2.age > 19")
            .yield("team.object as object").orderBy(null, orderBy,null).all();
        assert go.exist();
        assert go.count() == 2;
        List<ValueWrapper> object = all.colValues("object");
        assert object.get(0).asString().equals("chinese");
        assert object.get(1).asString().equals("math");
    }

    @Test
    public void testGoGetDstIDAddWhereYieldAddLimitAddOrderBy()
        throws UnsupportedEncodingException {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        Goer goer = new Goer(graph);
        HashMap<String, Sort> orderBy = new HashMap<>();
        orderBy.put("object", null);
        Go go = goer.go(ids, edges);
        ResultSet all = go.where(null, "$$.QKM2.age > 19")
            .yield("team.object as object").limit(0, 1)
            .orderBy(null, orderBy,null).all();
        assert go.exist();
        assert go.count() == 1;
        List<ValueWrapper> object = all.colValues("object");
        assert object.get(0).asString().equals("math");
    }

    @Test
    public void testGoGetDstIDAddWhereYieldAddIllegalLimitAddOrderBy() {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        Goer goer = new Goer(graph);
        HashMap<String, Sort> orderBy = new HashMap<>();
        orderBy.put("object", null);
        Go go = goer.go(ids, edges);
        try {
            ResultSet all = go.where(null, "$$.QKM2.age > 19")
                .yield("team.object as object").limit(-1, 1)
                .orderBy(null, orderBy,null).all();
        } catch (Exception e) {
           assert e.getMessage().equals("SyntaxError: syntax error near `-1,1  | '");
        }
    }

    @Test
    public void testGoGetDstIDYieldAddOrderByAddGroupBy() throws UnsupportedEncodingException {
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        HashMap<String, Sort> orderBy = new HashMap<>();
        orderBy.put("object", null);
        List<Column> groupBy = new ArrayList<>();
        List<Column> aggregateFunctions = new ArrayList<>();
        Column column = new Column("$-.object", "object");
        groupBy.add(column);
        Column aggregateColumn = new Column(AggregateFunction.COUNT.setValue("*"), "count");
        aggregateFunctions.add(aggregateColumn);
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.yield("team.object as object").orderBy(null, orderBy,null)
            .groupBy(groupBy, aggregateFunctions).all();
        assert go.exist();
        assert go.count() == 2;
        List<ValueWrapper> object = all.colValues("object");
        List<ValueWrapper> count = all.colValues("count");
        assert object.get(0).asString().equals("math")
            && object.get(1).asString().equals("chinese");
        assert count.get(0).asLong() == 2
            && count.get(1).asLong() == 1;
    }

    @Test
    public void testGoGetDstIDYieldAddOrderByAddGroupByAddOrderBy()
        throws UnsupportedEncodingException {
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        HashMap<String, Sort> orderBy = new HashMap<>();
        orderBy.put("object", null);
        List<Column> groupBy = new ArrayList<>();
        List<Column> aggregateFunctions = new ArrayList<>();
        Column column = new Column("$-.object", "object");
        groupBy.add(column);
        Column aggregateColumn = new Column(AggregateFunction.COUNT.setValue("*"), "count");
        aggregateFunctions.add(aggregateColumn);
        ArrayList<String> edges = new ArrayList<>();
        edges.add("team");
        edges.add("work");
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, edges);
        ResultSet all = go.yield("team.object as object").orderBy(null, orderBy,null)
            .groupBy(groupBy, aggregateFunctions).orderBy(null, orderBy,null).all();
        assert go.exist();
        assert go.count() == 3;
        List<ValueWrapper> object = all.colValues("object");
        List<ValueWrapper> count = all.colValues("count");
        assert object.get(0).isEmpty()
            && object.get(1).asString().equals("chinese")
            && object.get(2).asString().equals("math");
        assert count.get(0).asLong() == 3
            && count.get(1).asLong() == 1
            && count.get(2).asLong() == 2;
    }

    @Test
    public void testGoSrcIdsException() {
        ArrayList<Integer> ids = new ArrayList<>();
        Goer goer = new Goer(graph);
        Go go = goer.go(ids, null);
        try {
            ResultSet all = go.yield("team.object as object").all();
        } catch (Exception e) {
            assert e.getMessage().equals("srcIds can not be null");
        }
    }
}
