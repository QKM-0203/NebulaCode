/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.orm.match.VertexMatch;
import com.vesoft.nebula.orm.match.VertexMatcher;
import com.vesoft.nebula.orm.operator.*;
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
public class TestMatchVertex extends TestDataBase {

    {
        graph.createTag(qkm1);
        graph.createTag(qkm2);
        graph.create(vertexOne);
        graph.create(vertexTwo);
        graph.create(vertexThird);
        graph.create(vertexFour);
        graph.createTagIndex("QKM1", "i_QKM1", null);
        HashMap<String, Integer> indexOnQKM2 = new HashMap<>();
        graph.createTagIndex("QKM2", "i_QKM2", null);
        indexOnQKM2.put("name", 10);
        graph.createTagIndex("QKM2", "i_QKM2_name", indexOnQKM2);
        indexOnQKM2.put("age", null);
        graph.createTagIndex("QKM2", "i_QKM2_name_age", indexOnQKM2);
        indexOnQKM2.remove("name", 10);
        graph.createTagIndex("QKM2", "i_QKM2_age", indexOnQKM2);
        graph.run("REBUILD TAG INDEX i_QKM1, i_QKM2_name, i_QKM2, i_QKM2_name_age, "
            + "i_QKM2_age");
    }

    @Test
    public void testMatchByTagName() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch qkm1 = vertexMatcher.match("QKM1", null);
        ResultSet matchByTagQKM1 = qkm1.all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm1.exist();
        assert qkm1.count() == 4;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("QKM2").get("name").asString().equals("sy");
        assert nodes.get(3).asNode().properties("QKM2").get("name").asString().equals("yq");
    }

    @Test
    public void testMatchCount() {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch qkm1 = vertexMatcher.match("QKM1", null);
        ResultSet matchByTagQKM1 = qkm1.all();
        assert qkm1.exist();
        List<ValueWrapper> nodeWrapper = matchByTagQKM1.colValues("v");
        assert nodeWrapper.size() == qkm1.count();
    }

    @Test
    public void testMatchByTagNameGetFirst() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        ResultSet.Record qkm1 = vertexMatcher.match("QKM1", null).first();
        assert qkm1.get("v").asNode().properties("QKM2").get("name").asString().equals("sc");
    }

    @Test
    public void testMatchByTag() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Object> tag = new HashMap<>();
        tag.put("name", "qkm");
        VertexMatch qkm2 = vertexMatcher.match("QKM2", tag);
        ResultSet matchByTagQKM1 = qkm2.all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 1;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("qkm")
            && nodes.get(0).asNode().properties("QKM2").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereRelational() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", Relational.EQ.setValue("qkm"));
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 1;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("qkm")
            && nodes.get(0).asNode().properties("QKM2").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereRelational1() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("age", Relational.LE.setValue(19));
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 2;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("qkm")
            && nodes.get(0).asNode().properties("QKM2").get("age").asLong() == 19;
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("sc")
            && nodes.get(1).asNode().properties("QKM2").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereRelational2() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("age", Relational.LE.setValue(19));
        filter.put("name", Relational.EQ.setValue("qkm"));
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 1;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("qkm")
            && nodes.get(0).asNode().properties("QKM2").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereRelational3() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", Relational.CONTAINS.setValue("q"));
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 2;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("yq");
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("qkm");
    }

    @Test
    public void testMatchByTagNameAddWhereRelational4() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", Relational.STARTSWITH.setValue("q"));
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 1;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("qkm");
    }

    @Test
    public void testMatchByTagNameAddWhereRelational5() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", Relational.ENDSWITH.setValue("q"));
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 1;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("yq");
    }

    @Test
    public void testMatchByTagNameAddWhereRelational6() throws UnsupportedEncodingException {
        ArrayList<String> nameList = new ArrayList<>();
        nameList.add("qkm");
        nameList.add("sc");
        nameList.add("sy");
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", Relational.IN.setValue(nameList));
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 3;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("QKM2").get("name").asString().equals("sy");
    }

    @Test
    public void testMatchByTagNameAddWhereLogical() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("age", Logical.AND.setRelational(Relational.GT.setValue(18),
            Relational.LT.setValue(20)));
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 2;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("qkm");
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("sc");
    }

    @Test
    public void testMatchByTagNameAddWhereString() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(null, "v.name == \"qkm\"").all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 1;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("qkm")
            && nodes.get(0).asNode().properties("QKM2").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereStringLogical() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(null, "v.name == \"qkm\" or v.name ==\"sc\"").all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 2;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("qkm");
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("sc");
    }

    @Test
    public void testMatchByTagNameAddUnaryOperate() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", UnaryOperation.IsNotNull);
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 4;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("QKM2").get("name").asString().equals("sy");
        assert nodes.get(3).asNode().properties("QKM2").get("name").asString().equals("yq");
    }

    @Test
    public void testMatchByTagNameAddUnaryOperate1() {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", UnaryOperation.IsNull);
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(filter).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert !qkm2.exist();
        assert qkm2.count() == 0;
    }

    @Test
    public void testMatchByTagNameAddWhereAddLimit() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(null, "v.age <= 20").limit(2).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 2;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("qkm");
    }

    @Test
    public void testMatchByTagNameAddWhereAddLimitAddSkip() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(null, "v.age <= 20").limit(1).skip(2).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 1;
        assert  nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("sy");
    }

    @Test
    public void testMatchByTagNameAddWhereAddIllegalLimitAddIllegalSkip()
        throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(null, "v.age <= 20").limit(-1).skip(-1).all();
        List<ValueWrapper> nodes = matchByTagQKM1.colValues("v");
        assert qkm2.exist();
        assert qkm2.count() == 3;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("QKM2").get("name").asString().equals("sy");
    }

    @Test
    public void testMatchByTagNameAddWhereAddOrderBy() {
        HashMap<Column, Sort> orderBy = new HashMap<>();
        Column column = new Column("v.age", "age");
        orderBy.put(column, Sort.DESC);
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(null, "v.age <= 20")
            .orderBy(orderBy,null,null).all();
        List<ValueWrapper> outPut = matchByTagQKM1.colValues("age");
        assert qkm2.exist();
        assert qkm2.count() == 3;
        assert outPut.get(0).asLong() == 20;
        assert outPut.get(1).asLong() == 19;
        assert outPut.get(2).asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereAddGroupBy() {
        HashMap<Column, Sort> orderBy = new HashMap<>();
        Column column = new Column("v.age", "age");
        orderBy.put(column, Sort.DESC);
        List<Column> groupBy = new ArrayList<>();
        List<Column> aggregateFunctions = new ArrayList<>();
        groupBy.add(column);
        Column aggregateColumn = new Column(AggregateFunction.COUNT.setValue("*"), "count");
        aggregateFunctions.add(aggregateColumn);
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch qkm2 = vertexMatcher.match("QKM2", null);
        ResultSet matchByTagQKM1 = qkm2.where(null, "v.age <= 20")
            .orderBy(orderBy,null,null)
            .groupBy(groupBy, aggregateFunctions).all();
        List<ValueWrapper> outPut = matchByTagQKM1.colValues("age");
        assert qkm2.exist();
        assert qkm2.count() == 2;
        assert outPut.get(0).asLong() == 20;
        List<ValueWrapper> outPut1 = matchByTagQKM1.colValues("count");
        assert outPut1.get(0).asLong() == 1;
        assert outPut.get(1).asLong() == 19;
        assert outPut1.get(1).asLong() == 2;
    }

    @Test
    public void testMatchById() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        ResultSet.Record vertexByVid = vertexMatcher.getVertexByVid(1);
        ValueWrapper vertex = vertexByVid.get("v");
        assert vertexByVid.size() == 1;
        assert vertex.asNode().properties("QKM2").get("name").asString().equals("qkm");
    }

    @Test
    public void testMatchByIds() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        ids.add(9);
        ResultSet vertexByListVid = vertexMatcher.getVertexByListVid(ids);
        List<ValueWrapper> nodes = vertexByListVid.colValues("v");
        assert nodes.size() == 3;
        assert nodes.get(0).asNode().properties("QKM2").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("QKM2").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("QKM2").get("name").asString().equals("sy");
    }

}
