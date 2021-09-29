/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.Node;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.ngqlbuilder.match.VertexMatch;
import com.vesoft.nebula.ngqlbuilder.match.VertexMatcher;
import com.vesoft.nebula.ngqlbuilder.operator.*;
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
public class TestMatchVertex extends TestDataBase {

    {
        graph.createTag(person);
        graph.createTag(hobby);
        graph.create(vertexOne);
        graph.create(vertexTwo);
        graph.create(vertexThird);
        graph.create(vertexFour);
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
    public void testMatchByTagName() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch hobby = vertexMatcher.match("hobby", null);
        ResultSet matchByTaghobby = hobby.all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert hobby.exist();
        assert hobby.count() == 4;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("person").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("person").get("name").asString().equals("sy");
        assert nodes.get(3).asNode().properties("person").get("name").asString().equals("yq");
    }

    @Test
    public void testMatchCount() {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch hobby = vertexMatcher.match("hobby", null);
        ResultSet matchByTaghobby = hobby.all();
        assert hobby.exist();
        List<ValueWrapper> nodeWrapper = matchByTaghobby.colValues("v");
        assert nodeWrapper.size() == hobby.count();
    }

    @Test
    public void testMatchByTagNameGetFirst() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        ResultSet.Record hobby = vertexMatcher.match("hobby", null).first();
        assert hobby.get("v").asNode().properties("person").get("name").asString().equals("sc");
    }

    @Test
    public void testMatchByTag() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Object> tag = new HashMap<>();
        tag.put("name", "qkm");
        VertexMatch person = vertexMatcher.match("person", tag);
        ResultSet matchByTaghobby = person.all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 1;
        Node node = nodes.get(0).asNode();
        assert node.properties("person").get("name").asString().equals("qkm")
            && node.properties("person").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereRelational() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", Relational.EQ.setValue("qkm"));
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        Node node = nodes.get(0).asNode();
        assert person.exist();
        assert person.count() == 1;
        assert node.properties("person").get("name").asString().equals("qkm")
            && node.properties("person").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereRelational1() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("age", Relational.LE.setValue(19));
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        Node node = nodes.get(0).asNode();
        assert person.exist();
        assert person.count() == 2;
        assert node.properties("person").get("name").asString().equals("qkm")
            && node.properties("person").get("age").asLong() == 19;
        Node node1 = nodes.get(1).asNode();
        assert node1.properties("person").get("name").asString().equals("sc")
            && node1.properties("person").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereRelational2() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("age", Relational.LE.setValue(19));
        filter.put("name", Relational.EQ.setValue("qkm"));
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        Node node = nodes.get(0).asNode();
        assert person.exist();
        assert person.count() == 1;
        assert node.properties("person").get("name").asString().equals("qkm")
            && node.properties("person").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereRelational3() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", Relational.CONTAINS.setValue("q"));
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 2;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("yq");
        assert nodes.get(1).asNode().properties("person").get("name").asString().equals("qkm");
    }

    @Test
    public void testMatchByTagNameAddWhereRelational4() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", Relational.STARTSWITH.setValue("q"));
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 1;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("qkm");
    }

    @Test
    public void testMatchByTagNameAddWhereRelational5() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", Relational.ENDSWITH.setValue("q"));
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 1;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("yq");
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
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 3;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("person").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("person").get("name").asString().equals("sy");
    }

    @Test
    public void testMatchByTagNameAddWhereLogical() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("age", Logical.AND.setRelational(Relational.GT.setValue(18),
            Relational.LT.setValue(20)));
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 2;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("qkm");
        assert nodes.get(1).asNode().properties("person").get("name").asString().equals("sc");
    }

    @Test
    public void testMatchByTagNameAddWhereString() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(null, "v.name == \"qkm\"").all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        Node node = nodes.get(0).asNode();
        assert person.exist();
        assert person.count() == 1;
        assert node.properties("person").get("name").asString().equals("qkm")
            && node.properties("person").get("age").asLong() == 19;
    }

    @Test
    public void testMatchByTagNameAddWhereStringLogical() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(null, "v.name == \"qkm\" or v.name ==\"sc\"").all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 2;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("qkm");
        assert nodes.get(1).asNode().properties("person").get("name").asString().equals("sc");
    }

    @Test
    public void testMatchByTagNameAddUnaryOperate() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", UnaryOperation.IsNotNull);
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 4;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("person").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("person").get("name").asString().equals("sy");
        assert nodes.get(3).asNode().properties("person").get("name").asString().equals("yq");
    }

    @Test
    public void testMatchByTagNameAddUnaryOperate1() {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("name", UnaryOperation.IsNull);
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(filter).all();
        assert !person.exist();
        assert person.count() == 0;
        assert matchByTaghobby.isEmpty();
    }

    @Test
    public void testMatchByTagNameAddWhereAddLimit() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(null, "v.age <= 20").limit(2).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 2;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("person").get("name").asString().equals("qkm");
    }

    @Test
    public void testMatchByTagNameAddWhereAddLimitAddSkip() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(null, "v.age <= 20").limit(1).skip(2).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 1;
        assert  nodes.get(0).asNode().properties("person").get("name").asString().equals("sy");
    }

    @Test
    public void testMatchByTagNameAddWhereAddIllegalLimitAddIllegalSkip()
        throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(null, "v.age <= 20").limit(-1).skip(-1).all();
        List<ValueWrapper> nodes = matchByTaghobby.colValues("v");
        assert person.exist();
        assert person.count() == 3;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("person").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("person").get("name").asString().equals("sy");
    }

    @Test
    public void testMatchByTagNameAddWhereAddOrderBy() {
        HashMap<Column, Sort> orderBy = new HashMap<>();
        Column column = new Column("v.age", "age");
        orderBy.put(column, Sort.DESC);
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(null, "v.age <= 20")
            .orderBy(orderBy,null,null).all();
        List<ValueWrapper> outPut = matchByTaghobby.colValues("age");
        assert person.exist();
        assert person.count() == 3;
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
        VertexMatch person = vertexMatcher.match("person", null);
        ResultSet matchByTaghobby = person.where(null, "v.age <= 20")
            .orderBy(orderBy,null,null)
            .groupBy(groupBy, aggregateFunctions).all();
        List<ValueWrapper> outPut = matchByTaghobby.colValues("age");
        assert person.exist();
        assert person.count() == 2;
        assert outPut.get(0).asLong() == 20;
        List<ValueWrapper> outPut1 = matchByTaghobby.colValues("count");
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
        assert vertex.asNode().properties("person").get("name").asString().equals("qkm");
    }

    @Test
    public void testMatchByIds() throws UnsupportedEncodingException {
        VertexMatcher vertexMatcher = new VertexMatcher(graph);
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        ids.add(9);
        ResultSet vertexByListVid = vertexMatcher.getVertexByVid(ids);
        List<ValueWrapper> nodes = vertexByListVid.colValues("v");
        assert nodes.size() == 3;
        assert nodes.get(0).asNode().properties("person").get("name").asString().equals("sc");
        assert nodes.get(1).asNode().properties("person").get("name").asString().equals("qkm");
        assert nodes.get(2).asNode().properties("person").get("name").asString().equals("sy");
    }

}
