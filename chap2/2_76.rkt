#lang racket


;显式分派:
;无论是新增操作还是新增类型都是不好的,都需要修改大量代码

;数据导向:
;无论是增加新类型, 还是新操作, 都不用改动现有代码, 直接往表格里添加对应项就行了

;消息传递:
;将数据对象和其所需的操作整合在一起,因此它可以很方便地增加新类型。
;当新增操作时,需要修改大量涉及此操作的类型