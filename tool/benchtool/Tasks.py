bst = {
    'base': [],
    'insert_1': [
        'InsertPost',
        'InsertModel',
        'DeleteInsert',
        'InsertInsert',
        'InsertUnion',
        'UnionDeleteInsert',
    ],
    'insert_2': [
        'InsertPost',
        'InsertModel',
        'InsertDelete',
        'DeleteInsert',
        'InsertInsert',
        'InsertUnion',
        'UnionDeleteInsert',
    ],
    'insert_3': [
        'InsertPost',
        'InsertModel',
        'InsertDelete',
        'InsertInsert',
        'InsertUnion',
        'UnionDeleteInsert',
    ],
    'delete_4': [
        'DeleteModel',
        'DeletePost',
        'DeleteDelete',
        'DeleteInsert',
        'DeleteUnion',
        'InsertDelete',
        'UnionDeleteInsert',
    ],
    'delete_5': [
        'DeleteModel',
        'DeletePost',
        'DeleteDelete',
        'DeleteInsert',
        'DeleteUnion',
        'UnionDeleteInsert',
    ],
    'union_6': [
        'UnionValid',
        'UnionPost',
        'UnionModel',
        'DeleteUnion',
        'InsertUnion',
        'UnionDeleteInsert',
        'UnionUnionAssoc',
        'UnionUnionIdem',
    ],
    'union_7': [
        'UnionValid',
        'UnionPost',
        'UnionModel',
        'DeleteUnion',
        'InsertUnion',
        'UnionDeleteInsert',
        'UnionUnionAssoc',
    ],
    'union_8': [
        'UnionPost',
        'UnionModel',
        'DeleteUnion',
        'InsertUnion',
        'UnionDeleteInsert',
        'UnionUnionAssoc',
    ],
}

redblack = {
    'miscolor_insert': [
        'InsertValid',
        'DeleteInsert',
    ],
    'miscolor_delete': ['DeleteValid'],
    'miscolor_balLeft': [
        'DeleteValid',
        'DeleteDelete',
    ],
    'miscolor_balRight': [
        'DeleteValid',
        'DeleteDelete',
    ],
    'miscolor_join_1': ['DeleteValid'],
    'miscolor_join_2': [
        'DeleteValid',
        'DeleteDelete',
    ],
    'no_balance_insert_1': [
        'InsertValid',
        'DeleteInsert',
        'InsertDelete',
    ],
    'no_balance_insert_2': [
        'InsertValid',
        'DeleteInsert',
        'InsertDelete',
    ],
    'swap_cd': [
        'InsertValid',
        'InsertModel',
        'InsertPost',
        'DeleteValid',
        'DeletePost',
        'DeleteModel',
        'DeleteDelete',
        'DeleteInsert',
        'InsertDelete',
        'InsertInsert',
    ],
    'swap_bc': [
        'InsertValid',
        'InsertModel',
        'InsertPost',
        'DeleteValid',
        'DeletePost',
        'DeleteModel',
        'DeleteDelete',
        'DeleteInsert',
        'InsertDelete',
        'InsertInsert',
    ],
}

tasks = {'BST': bst, 'RBT': {**bst, **redblack}}