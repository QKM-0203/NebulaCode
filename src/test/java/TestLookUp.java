/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.orm.operator.Filter;
import com.vesoft.nebula.orm.operator.Relational;
import com.vesoft.nebula.orm.operator.Sort;
import com.vesoft.nebula.orm.query.ngql.LookUp;
import com.vesoft.nebula.orm.query.ngql.LookerUp;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.List;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestLookUp extends TestDataBase {
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
        graph.createEdgeIndex("work", "i_work", null);
        graph.createEdgeIndex("team", "i_team", null);
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        graph.run("REBUILD EDGE INDEX i_work, i_team");
        graph.createTagIndex("QKM1", "i_QKM1", null);
        graph.createTagIndex("QKM2", "i_QKM2", null);
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        graph.run("REBUILD TAG INDEX i_QKM1,i_QKM2");
    }

    @Test
    public void testLookUpOnTag() {
        LookerUp lookerUp = new LookerUp(graph);
        LookUp qkm21 = lookerUp.lookUp("QKM2");
        ResultSet qkm2 = qkm21.all();
        assert qkm21.exist();
        assert qkm21.count() == 4;
        List<ValueWrapper> vertexID = qkm2.colValues("VertexID");
        assert vertexID.get(0).asLong() == 1;
        assert vertexID.get(1).asLong() == 2;
        assert vertexID.get(2).asLong() == 3;
        assert vertexID.get(3).asLong() == 4;
    }

    @Test
    public void testLookUpOnEdge() {
        LookerUp lookerUp = new LookerUp(graph);
        LookUp team = lookerUp.lookUp("team");
        ResultSet qkm2 = team.all();
        assert team.exist();
        assert team.count() == 3;
        List<ValueWrapper> srcVID = qkm2.colValues("SrcVID");
        List<ValueWrapper> dstVid = qkm2.colValues("DstVID");
        List<ValueWrapper> ranking = qkm2.colValues("Ranking");
        assert srcVID.get(0).asLong() == 1;
        assert dstVid.get(0).asLong() == 2;
        assert ranking.get(0).asLong() == 1;
        assert srcVID.get(1).asLong() == 2;
        assert dstVid.get(1).asLong() == 3;
        assert ranking.get(1).asLong() == 0;
        assert srcVID.get(2).asLong() == 2;
        assert dstVid.get(2).asLong() == 4;
        assert ranking.get(2).asLong() == 1;
    }

    @Test
    public void testLookUpOnTagAddWhere() {
        LookerUp lookerUp = new LookerUp(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("QKM2.age", Relational.LE.setValue(19));
        LookUp qkm21 = lookerUp.lookUp("QKM2");
        ResultSet qkm2 = qkm21.where(filter).all();
        assert qkm21.exist();
        assert qkm21.count() == 2;
        List<ValueWrapper> vertexID = qkm2.colValues("VertexID");
        assert vertexID.get(0).asLong() == 1;
        assert vertexID.get(1).asLong() == 2;
    }

    @Test
    public void testLookUpOnEdgeAddWhere() {
        LookerUp lookerUp = new LookerUp(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("team.object", Relational.EQ.setValue("math"));
        LookUp team = lookerUp.lookUp("team");
        ResultSet qkm2 = team.where(filter).all();
        assert team.exist();
        assert team.count() == 2;
        List<ValueWrapper> srcVid = qkm2.colValues("SrcVID");
        List<ValueWrapper> dstVid = qkm2.colValues("DstVID");
        List<ValueWrapper> ranking = qkm2.colValues("Ranking");
        assert srcVid.get(0).asLong() == 1;
        assert dstVid.get(0).asLong() == 2;
        assert ranking.get(0).asLong() == 1;
        assert srcVid.get(1).asLong() == 2;
        assert dstVid.get(1).asLong() == 3;
        assert ranking.get(1).asLong() == 0;
    }

    @Test
    public void testLookUpOnTagAddWhereAddYield() throws UnsupportedEncodingException {
        LookerUp lookerUp = new LookerUp(graph);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("QKM2.age", Relational.LE.setValue(19));
        LookUp qkm21 = lookerUp.lookUp("QKM2");
        ResultSet qkm2 = qkm21.where(filter).yield("DISTINCT QKM2.name as name").all();
        assert qkm21.exist();
        assert qkm21.count() == 2;
        List<ValueWrapper> vertexIDs = qkm2.colValues("VertexID");
        List<ValueWrapper> names = qkm2.colValues("name");
        assert vertexIDs.get(0).asLong() == 1;
        assert names.get(0).asString().equals("qkm");
        assert vertexIDs.get(1).asLong() == 2;
        assert names.get(1).asString().equals("sc");
    }

    @Test
    public void testLookUpOnTagAddWhereAddYieldAddOrderByAddLimit() throws UnsupportedEncodingException {
        LookerUp lookerUp = new LookerUp(graph);
        HashMap<String, Sort> orderBy = new HashMap<>();
        orderBy.put("name", Sort.ASC);
        HashMap<String, Filter> filter = new HashMap<>();
        filter.put("QKM2.age", Relational.LE.setValue(19));
        LookUp qkm21 = lookerUp.lookUp("QKM2");
        ResultSet qkm2 = qkm21.where(filter).yield("DISTINCT QKM2.name as name")
            .orderBy(null,orderBy,null).limit(1,1).all();
        assert qkm21.exist();
        assert qkm21.count() == 1;
        List<ValueWrapper> vertexIDs = qkm2.colValues("VertexID");
        List<ValueWrapper> names = qkm2.colValues("name");
        assert vertexIDs.get(0).asLong() == 2;
        assert names.get(0).asString().equals("sc");
    }

    @Test
    public void testLookUpSchemaException() {
        LookerUp lookerUp = new LookerUp(graph);
        LookUp qkm21 = lookerUp.lookUp(null);
        try {
            ResultSet qkm2 = qkm21.yield("DISTINCT QKM2.name as name").all();
        } catch (Exception e) {
            assert e.getMessage().equals("schema can not be null");
        }
    }
}
