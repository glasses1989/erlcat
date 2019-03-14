//
//  erlcat.c
//
//  Created by zengxiantao951 on 2018/12/27.
//  Copyright © 2018 pingan. All rights reserved.
//
#include <stdio.h>
#include <string.h>

#include "../include/client.h"
#include "erl_nif.h"

#define MAXKEYLEN 128
#define MAXVALLEN 1024

static ErlNifResourceType* cattrans_res;

typedef struct _transaction_t {
    CatTransaction* _trans;
} transaction_t;

static ERL_NIF_TERM make_atom(ErlNifEnv *env, const char *atom_name) {
    ERL_NIF_TERM atom;
    if(enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1)) {
        return atom;
    }
    return enif_make_atom(env, atom_name);
}

//Common Apis.

// 初始化cat实例.
ERL_NIF_TERM initCatClient(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char appKey[MAXKEYLEN];
    CatClientConfig config = DEFAULT_CCAT_CONFIG;

    (void)memset(&appKey, '\0', sizeof(appKey));
    
    if (enif_get_string(env, argv[0], appKey, sizeof(appKey), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
//    config.enableHeartbeat = 0;
//    config.enableDebugLog = 0;
    // 不能设置为字符编码类型.
//    config.encoderType = 0;

    if (!enif_get_int(env, argv[1], &config.encoderType)) {
            return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[2], &config.enableHeartbeat)) {
                return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[3], &config.enableSampling)) {
                return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[4], &config.enableMultiprocessing)) {
                return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[5], &config.enableDebugLog)) {
                return enif_make_badarg(env);
    }

    return enif_make_int(env, catClientInitWithConfig(appKey, &config));
}

// 返回cat版本.
ERL_NIF_TERM getCatVersion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_string(env, catVersion(), ERL_NIF_LATIN1);
}

// 返回cat是否可用（是否被初始化）.
ERL_NIF_TERM isCatEnabledOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_int(env, isCatEnabled());
}

// 销毁cat实例，释放所有cat已经申请的资源.
ERL_NIF_TERM destroyCatClient(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_int(env, catClientDestroy());
}

//Event Apis.

// 记录Event.
ERL_NIF_TERM logEventOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char type[MAXKEYLEN];
    (void)memset(&type, '\0', sizeof(type));
    
    if (enif_get_string(env, argv[0], type, sizeof(type), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    char name[MAXKEYLEN];
    (void)memset(&name, '\0', sizeof(name));
    
    if (enif_get_string(env, argv[1], name, sizeof(name), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    char status[MAXKEYLEN];
    (void)memset(&status, '\0', sizeof(status));
    
    if (enif_get_string(env, argv[2], status, sizeof(status), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    char data[MAXVALLEN];
    (void)memset(&data, '\0', sizeof(data));
    
    if (enif_get_string(env, argv[3], data, sizeof(data), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    logEvent(type, name, status, data);
    return make_atom(env, "ok");
}

// 记录错误Event.
ERL_NIF_TERM logErrorOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char msg[MAXKEYLEN];
    (void)memset(&msg, '\0', sizeof(msg));
    
    if (enif_get_string(env, argv[0], msg, sizeof(msg), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    char errStr[MAXVALLEN];
    (void)memset(&errStr, '\0', sizeof(errStr));
    
    if (enif_get_string(env, argv[1], errStr, sizeof(errStr), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    logError(msg, errStr);
    return make_atom(env, "ok");
}

// 记录count类型Metric.
ERL_NIF_TERM logMetricForCountOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char name[MAXKEYLEN];
    (void)memset(&name, '\0', sizeof(name));
    
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    int count;
    if(!enif_get_int(env, argv[1], &count)) {
        return enif_make_badarg(env);
    }
    
    logMetricForCount(name, count);
    return make_atom(env, "ok");
}

// 记录duration类型Metric.
ERL_NIF_TERM logMetricForDurationOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char name[MAXKEYLEN];
    (void)memset(&name, '\0', sizeof(name));
    
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    long duration;
    if(!enif_get_long(env, argv[1], &duration)) {
        return enif_make_badarg(env);
    }
    
    logMetricForDuration(name, duration);
    return make_atom(env, "ok");
}

// 记录sum类型(等价count类型)Metric.
ERL_NIF_TERM logMetricForSumOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char name[MAXKEYLEN];
    (void)memset(&name, '\0', sizeof(name));
    
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    int count;
    if(!enif_get_int(env, argv[1], &count)) {
        return enif_make_badarg(env);
    }
    
    logMetricForCount(name, count);
    return make_atom(env, "ok");
}

//Transaction Apis.

// 记录耗时类的Transaction.
ERL_NIF_TERM logTransactionWithDurationOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char type[MAXKEYLEN];
    (void)memset(&type, '\0', sizeof(type));
    
    if (enif_get_string(env, argv[0], type, sizeof(type), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    char name[MAXKEYLEN];
    (void)memset(&name, '\0', sizeof(name));
    
    if (enif_get_string(env, argv[1], name, sizeof(name), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    long duration;
    if(!enif_get_long(env, argv[2], &duration)) {
        return enif_make_badarg(env);
    }
    
    newCompletedTransactionWithDuration(type, name, duration);
    return make_atom(env, "ok");
}

// 创建Transaction对象.
ERL_NIF_TERM newTransactionOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    transaction_t* trans_t;
    ERL_NIF_TERM retterm;
    
    char type[MAXKEYLEN];
    (void)memset(&type, '\0', sizeof(type));
    
    if (enif_get_string(env, argv[0], type, sizeof(type), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }

    char name[MAXKEYLEN];
    (void)memset(&name, '\0', sizeof(name));
    
    if (enif_get_string(env, argv[1], name, sizeof(name), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }

    trans_t = enif_alloc_resource(cattrans_res, sizeof(transaction_t));
    if(trans_t == NULL) return enif_make_badarg(env);
    retterm = enif_make_resource(env, trans_t);
	trans_t->_trans = newTransaction(type, name);
    return retterm;
}

// 设置Transaction状态.
ERL_NIF_TERM setStatusForTransaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    transaction_t* trans_t;
    if (!enif_get_resource(env, argv[0], cattrans_res, (void**) &trans_t)) {
        return enif_make_badarg(env);
    }
    
    char status[MAXKEYLEN];
    (void)memset(&status, '\0', sizeof(status));
    if (enif_get_string(env, argv[1], status, sizeof(status), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    CatTransaction *trans = trans_t->_trans;
    trans->setStatus(trans, status);
    return make_atom(env, "ok");
}

// 设置Transaction时间戳
ERL_NIF_TERM setTimestampForTransaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    transaction_t* trans_t;
    if (!enif_get_resource(env, argv[0], cattrans_res, (void**) &trans_t)) {
        return enif_make_badarg(env);
    }
    
    long timestamp;
    if(!enif_get_long(env, argv[1], &timestamp)) {
        return enif_make_badarg(env);
    }
    
    CatTransaction *trans = trans_t->_trans;
    trans->setTimestamp(trans, timestamp);
    return make_atom(env, "ok");
}

// 设置Transaction持续时间.
ERL_NIF_TERM setDurationForTransaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    transaction_t* trans_t;
    if (!enif_get_resource(env, argv[0], cattrans_res, (void**) &trans_t)) {
        return enif_make_badarg(env);
    }
    
    long duration;
    if(!enif_get_long(env, argv[1], &duration)) {
        return enif_make_badarg(env);
    }
    
    CatTransaction *trans = trans_t->_trans;
    trans->setDurationInMillis(trans, duration);
    return make_atom(env, "ok");
}

// 设置Transaction持续的起始时间.
ERL_NIF_TERM setDurationStartForTransaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    transaction_t* trans_t;
    if (!enif_get_resource(env, argv[0], cattrans_res, (void**) &trans_t)) {
        return enif_make_badarg(env);
    }
    
    long durationStart;
    if(!enif_get_long(env, argv[1], &durationStart)) {
        return enif_make_badarg(env);
    }
    
    CatTransaction *trans = trans_t->_trans;
    trans->setDurationStart(trans, durationStart);
    return make_atom(env, "ok");
}

// 添加Transaction附加数据.
ERL_NIF_TERM addDataForTransaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    transaction_t* trans_t;
    if (!enif_get_resource(env, argv[0], cattrans_res, (void**) &trans_t)) {
        return enif_make_badarg(env);
    }
    
    char data[MAXVALLEN];
    (void)memset(&data, '\0', sizeof(data));
    if (enif_get_string(env, argv[1], data, sizeof(data), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    CatTransaction *trans = trans_t->_trans;
    trans->addData(trans, data);
    return make_atom(env, "ok");
}

// 添加Transaction附加数据（key-value格式）.
ERL_NIF_TERM addKeyValueForTransaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    transaction_t* trans_t;
    if (!enif_get_resource(env, argv[0], cattrans_res, (void**) &trans_t)) {
        return enif_make_badarg(env);
    }
    
    char key[MAXKEYLEN];
    (void)memset(&key, '\0', sizeof(key));
    if (enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    char value[MAXVALLEN];
    (void)memset(&value, '\0', sizeof(value));
    if (enif_get_string(env, argv[2], value, sizeof(value), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    CatTransaction *trans = trans_t->_trans;
    trans->addKV(trans, key, value);
    return make_atom(env, "ok");
}

// 提交Transaction.
ERL_NIF_TERM completeForTransaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    CatTransaction *trans;
    transaction_t* trans_t;
    if (!enif_get_resource(env, argv[0], cattrans_res, (void**) &trans_t)) {
        return enif_make_badarg(env);
    }
    
    trans = trans_t->_trans;
    trans->complete(trans);
    trans_t->_trans = NULL;
    enif_release_resource(trans_t);
    return make_atom(env, "ok");
}

// MessageID Apis.

// 创建messageId.
ERL_NIF_TERM createMessageIdOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_string(env, createMessageId(), ERL_NIF_LATIN1);
}

// 创建messageId(指定远程服务).
ERL_NIF_TERM createRemoteMessageIdOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char appKey[MAXKEYLEN];
    (void)memset(&appKey, '\0', sizeof(appKey));

    if (enif_get_string(env, argv[0], appKey, sizeof(appKey), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }

    return enif_make_string(env, createRemoteServerMessageId(appKey), ERL_NIF_LATIN1);
}

// 获取本地messageTreeId.
ERL_NIF_TERM getMessageTreeIdOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_string(env, getThreadLocalMessageTreeId(), ERL_NIF_LATIN1);
}

// 获取本地messageTreeRootId.
ERL_NIF_TERM getMessageTreeRootIdOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_string(env, getThreadLocalMessageTreeRootId(), ERL_NIF_LATIN1);
}

// 获取本地messageTreeParentId.
ERL_NIF_TERM getMessageTreeParentIdOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_string(env, getThreadLocalMessageTreeParentId(), ERL_NIF_LATIN1);
}

// 设置本地messageTreeId.
ERL_NIF_TERM setMessageTreeIdOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char messageId[MAXKEYLEN];
    (void)memset(&messageId, '\0', sizeof(messageId));

    if (enif_get_string(env, argv[0], messageId, sizeof(messageId), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    setThreadLocalMessageTreeId(messageId);
    return make_atom(env, "ok");
}

// 设置本地messageTreeId.
ERL_NIF_TERM setMessageTreeRootIdOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char messageId[MAXKEYLEN];
    (void)memset(&messageId, '\0', sizeof(messageId));

    if (enif_get_string(env, argv[0], messageId, sizeof(messageId), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    setThreadLocalMessageTreeRootId(messageId);
    return make_atom(env, "ok");
}

// 设置本地messageTreeId.
ERL_NIF_TERM setMessageTreeParentIdOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char messageId[MAXKEYLEN];
    (void)memset(&messageId, '\0', sizeof(messageId));

    if (enif_get_string(env, argv[0], messageId, sizeof(messageId), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    
    setThreadLocalMessageTreeParentId(messageId);
    return make_atom(env, "ok");
}

//Resource Load.

static void transaction_destruct(ErlNifEnv* env, void *obj) {
//    transaction_t *trans_t = (transaction_t*)obj;
//    free(trans_t->_trans);
//    free(trans_t);
//    enif_release_resource(obj);
}

// 设置本地messageTreeId.
ERL_NIF_TERM logHeartbeatOfErlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char heartCategory[MAXKEYLEN];
    ERL_NIF_TERM key, value;
    ErlNifMapIterator iter;
    char hbKeyName[MAXKEYLEN];
    char hbValue[MAXKEYLEN];

    (void)memset(&heartCategory, '\0', sizeof(heartCategory));


	if (enif_get_string(env, argv[0], heartCategory, sizeof(heartCategory), ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }
    keyTail=argv[1];
    while(enif_get_list_cell(env,keyTail,&keyHead,&keyTail)){
		if(!enif_get_map_value(env,argv[2],keyHead,&mapValue)){
			return enif_make_badarg(env);
		}
		if(!enif_get_int(env,mapValue,&hbValue)){
			return enif_make_badarg(env);
		}

    }

	ezxml_t xml = ezxml_new_d("status");
	ezxml_t ext = ezxml_add_child_d(xml, "extension", 0);
	ezxml_set_attr_d(ext, "id", heartCategory);
	ezxml_t desc = ezxml_add_child_d(ext, "description", 0);
	char categoryDesc[MAXKEYLEN];
	strcpy(categoryDesc,"<![CDATA[");
	strcat(categoryDesc,heartCategory);
	strcat(categoryDesc,"]]>");
	ezxml_set_txt_d(desc, categoryDesc);
	int count = 1;
    enif_map_iterator_create(env, argv[1],&iter,ERL_NIF_MAP_ITERATOR_FIRST);
    while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
    	(void)memset(&hbKeyName, '\0', sizeof(hbKeyName));
    	(void)memset(&hbValue, '\0', sizeof(hbKeyName));
    	if(enif_get_string(env,key,hbKeyName,sizeof(hbKeyName),ERL_NIF_LATIN1)<1){
			goto badarg;
    	}
    	if(enif_get_string(env,key,hbValue,sizeof(hbValue),ERL_NIF_LATIN1)<1){
			goto badarg;
		}
        ezxml_t detail = ezxml_add_child_d(ext, "extensionDetail", count);
		ezxml_set_attr_d(detail, "id", hbKeyName);
		ezxml_set_attr_d(detail, "value", hbValue);
		count++;
        enif_map_iterator_next(env, &iter);
    }
    enif_map_iterator_destroy(env, &iter);

    char *xmlContent = ezxml_toxml(xml);
    ezxml_free(xml);
    CatHeartBeat *h = newHeartBeat("Heartbeat", g_cat_messageManager.ip);
	h->addData(h, xmlContent);
	free(xmlContent);
	h->complete(h);

    return make_atom(env, "ok");
badarg:
	enif_map_iterator_destroy(env, &iter);
	ezxml_free(xml);
	return enif_make_badarg(env);
}


static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
    if (!cattrans_res) {
        cattrans_res = enif_open_resource_type(env, "erlcat", "erlcat_transaction", &transaction_destruct, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
        if(!cattrans_res) {
            return -1;
        }
    }
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"init_cat", 6, initCatClient},
    
    {"get_cat_version", 0, getCatVersion},
    {"is_cat_enabled", 0, isCatEnabledOfErlang},
    {"destroy_cat", 0, destroyCatClient},

    {"log_event", 4, logEventOfErlang},
    {"log_error", 2, logErrorOfErlang},
    {"log_metric_for_count", 2, logMetricForCountOfErlang},
    {"log_metric_for_duration", 2, logMetricForDurationOfErlang},
    {"log_metric_for_sum", 2, logMetricForSumOfErlang},
    
    {"log_transaction_with_duration", 3, logTransactionWithDurationOfErlang},
    
    {"new_transaction", 2, newTransactionOfErlang},
    {"set_status", 2, setStatusForTransaction},
    {"set_timestamp", 2, setTimestampForTransaction},
    {"set_duration", 2, setDurationForTransaction},
    {"set_duration_start", 2,setDurationStartForTransaction},
    {"add_data", 2, addDataForTransaction},
    {"add_kv", 3, addKeyValueForTransaction},
    {"complete", 1, completeForTransaction},
    
    {"create_message_id", 0, createMessageIdOfErlang},
    {"create_remote_message_id", 1, createRemoteMessageIdOfErlang},
    {"get_message_tree_id", 0, getMessageTreeIdOfErlang},
    {"get_message_tree_root_id", 0, getMessageTreeRootIdOfErlang},
    {"get_message_tree_parent_id", 0, getMessageTreeParentIdOfErlang},
    {"set_message_tree_id", 1, setMessageTreeIdOfErlang},
    {"set_message_tree_root_id", 1, setMessageTreeRootIdOfErlang},
    {"set_message_tree_parent_id", 1, setMessageTreeParentIdOfErlang},

    {"log_heartbeat",2,logHeartbeatOfErlang}
};

ERL_NIF_INIT(erlcat, nif_funcs, &load, NULL, NULL, NULL);

